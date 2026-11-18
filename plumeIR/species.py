"""
The species registry: every gas and aerosol plumeIR knows, in one place
(2026-11-17, developer).

One :class:`Species` per name, with its phase, chemical formula, molar mass
and elements (gases), and for an aerosol its type, refractive-index source,
molecular components and density rule.  The older lists in
:mod:`plumeIR.constants` (``RFM_GASES``, ``POSSIBLE_GASES``,
``POSSIBLE_AEROSOLS``, ``WEIGHTS``) are views of this registry.

Names never repeat across phases: an acid aerosol is ``<ACID>_aq`` (an acid-
water solution), so ``HNO3`` is only the gas.

**Aerosol components.**  The retrieved mass column of an aerosol is the mass
of its particles.  Its molecular content follows from its components:

- *solution* (``H2SO4_aq``, ``HNO3_aq``): the acid at weight fraction *w*
  and water at 1 − *w*.  *w* is the fitted or held parameter
  ``wt_<name>`` where the aerosol has one (``H2SO4_aq``), otherwise fixed by
  its refractive-index source (``HNO3_aq``: Biermann et al. 2000, 50 wt%);
- *pure* (``WATER``, ``ICE``): one component, the whole particle;
- *mineral* and *carbon* (``ASH``, ``DUST``, ``SOOT``, ``OC``): no
  molecular component; mass only.

:func:`component_masses` and :func:`component_columns` turn a particle mass
column into component mass columns [g m⁻²] and molecular columns
[molec cm⁻²], comparable with gas columns (acid moles against SO2).

**Densities and refractive indices are owned by mieEODG**, which computes the
mass extinction from them; the registry records which rule applies and
:func:`density` asks mieEODG for the value, never a copy.

Molar masses are the IUPAC 2021 values plumeIR has used since v3.0 (the
former ``constants.WEIGHTS``); the formulas give the elements, and a test
checks the two agree.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from types import MappingProxyType
from typing import Mapping

import numpy as np

#: Avogadro constant [mol⁻¹].
N_A = 6.02214076e23

GAS = "gas"
AEROSOL = "aerosol"
AEROSOL_TYPES = ("solution", "pure", "mineral", "carbon", "generic")

#: Standard atomic weights [g mol⁻¹] (IUPAC 2021, abridged), for elements
#: and for the consistency check of the molar masses below.
ATOMIC_WEIGHTS: Mapping[str, float] = MappingProxyType({
    "H": 1.008, "C": 12.011, "N": 14.007, "O": 15.999, "F": 18.998, "Si": 28.085,
    "S": 32.06, "Cl": 35.45, "Br": 79.904, "I": 126.904,
})

#: Molar masses [g mol⁻¹] of the molecules plumeIR uses, gases and aerosol
#: components alike (the values of the former ``constants.WEIGHTS``).
MOLAR_MASS: Mapping[str, float] = MappingProxyType({
    "H2O": 18.01528, "CO2": 44.009, "O3": 47.997, "N2O": 44.013, "CH4": 16.043,
    "O2": 31.998, "SO2": 64.064, "SO3": 80.063, "HCl": 36.461, "HF": 20.006,
    "CO": 28.010, "OCS": 60.075, "NH3": 17.031, "NO": 30.006, "NO2": 46.005,
    "HNO3": 63.013, "HBr": 80.912, "HI": 127.912, "N2": 28.014, "ClO": 51.452,
    "SF6": 146.055, "OH": 17.007, "H2S": 34.081, "H2SO4": 98.079,
})

_TOKEN = re.compile(r"([A-Z][a-z]?)(\d*)")


def parse_formula(formula: str) -> dict[str, int]:
    """
    Element counts of a simple formula (no brackets or charges).

    Parameters
    ----------
    formula : str
        e.g. ``'H2SO4'``, ``'ClO'``, ``'OCS'``.

    Returns
    -------
    dict
        Element → count.

    Raises
    ------
    ValueError
        For a string that is not a formula of known elements.
    """
    counts: dict[str, int] = {}
    pos = 0
    for m in _TOKEN.finditer(formula):
        if m.start() != pos:
            break
        el, n = m.group(1), int(m.group(2) or 1)
        if el not in ATOMIC_WEIGHTS:
            raise ValueError(f"{formula!r}: unknown element {el!r}")
        counts[el] = counts.get(el, 0) + n
        pos = m.end()
    if pos != len(formula) or not counts:
        raise ValueError(f"{formula!r} is not a formula of known elements")
    return counts


def formula_mass(formula: str) -> float:
    """Molar mass from the atomic weights [g mol⁻¹] (for checks)."""
    return float(sum(ATOMIC_WEIGHTS[el] * n for el, n in parse_formula(formula).items()))


@dataclass(frozen=True)
class Component:
    """
    A molecular component of an aerosol.

    Attributes
    ----------
    formula : str
        The molecule (a key of :data:`MOLAR_MASS`).
    fraction : float or None
        Its mass fraction of the particle when fixed; ``None`` when it is
        given per spectrum by ``parameter``, or is the remainder.
    parameter : str or None
        The parameter holding its mass fraction (``wt_H2SO4_aq``).
    remainder : bool
        Its fraction is 1 minus the others' (the water of a solution).
    """
    formula:   str
    fraction:  float | None = None
    parameter: str | None = None
    remainder: bool = False


@dataclass(frozen=True)
class Species:
    """
    One gas or aerosol.

    Attributes
    ----------
    name : str
        The name used in configs and results.
    phase : {'gas', 'aerosol'}
    formula : str or None
        The molecule of a gas.
    rfm : bool
        A gas with HITRAN lines, handled by RFM.
    aerosol_type : str or None
        One of :data:`AEROSOL_TYPES`.
    ri_source : str or None
        mieEODG ARIA key of the refractive index (``ARIA_DEFAULTS``); ``None``
        where none is available (the species is deferred).
    components : tuple of Component
        Molecular content of an aerosol particle (empty: mass only).
    density_rule : str
        How mieEODG sets the particle density (documentation; the value is
        :func:`density`).
    description : str
    """
    name:         str
    phase:        str
    formula:      str | None = None
    rfm:          bool = False
    aerosol_type: str | None = None
    ri_source:    str | None = None
    components:   tuple = field(default_factory=tuple)
    density_rule: str = ""
    description:  str = ""

    @property
    def is_gas(self) -> bool:
        return self.phase == GAS

    @property
    def is_aerosol(self) -> bool:
        return self.phase == AEROSOL

    @property
    def molar_mass(self) -> float | None:
        """Of a gas [g mol⁻¹]; ``None`` for an aerosol."""
        return MOLAR_MASS[self.formula] if self.formula else None

    @property
    def elements(self) -> dict[str, int]:
        """Element counts of a gas molecule (empty for an aerosol)."""
        return parse_formula(self.formula) if self.formula else {}

    @property
    def solute(self) -> Component | None:
        """The acid of a solution aerosol."""
        return next((c for c in self.components if not c.remainder), None) \
            if self.aerosol_type == "solution" else None


def _gas(name: str, description: str = "") -> Species:
    return Species(name, GAS, formula=name, rfm=True, description=description)


_GASES = ["H2O", "CO2", "SO2", "CO", "NO", "O3", "N2O", "NO2", "NH3", "HNO3",
          "CH4", "O2", "HCl", "HF", "HBr", "HI", "H2S", "N2", "ClO", "SF6",
          "SO3", "OCS", "OH"]

_AEROSOLS = [
    Species("H2SO4_aq", AEROSOL, aerosol_type="solution", ri_source="acids/sulphuric",
            components=(Component("H2SO4", parameter="wt_H2SO4_aq"),
                        Component("H2O", remainder=True)),
            density_rule="mieEODG h2so4_density(w) (Lund Myhre et al. 2003)",
            description="sulphuric acid aerosol: H2SO4-H2O solution droplets; "
                        "2-D table over radius and weight fraction"),
    Species("HNO3_aq", AEROSOL, aerosol_type="solution", ri_source="acids/nitric",
            components=(Component("HNO3", fraction=0.50), Component("H2O", remainder=True)),
            density_rule="mieEODG hno3_density(w), w = 0.50 from the refractive "
                         "index (Biermann et al. 2000, 293 K)",
            description="nitric acid aerosol: HNO3-H2O solution droplets, fixed 50 wt%"),
    Species("WATER", AEROSOL, aerosol_type="pure", ri_source="water_and_ice/water",
            components=(Component("H2O", fraction=1.0),),
            density_rule="mieEODG DENSITY_WATER (1.000 g cm⁻³)",
            description="liquid water droplets (Hale and Querry 1973, 298 K)"),
    Species("ICE", AEROSOL, aerosol_type="pure", ri_source="water_and_ice/ice",
            components=(Component("H2O", fraction=1.0),),
            density_rule="mieEODG ice_density(T) at the refractive index's 266 K",
            description="water ice, Mie spheres (Warren and Brandt 2008, 266 K)"),
    Species("ASH", AEROSOL, aerosol_type="mineral", ri_source="volcanic_ash",
            density_rule="mieEODG DENSITY_ASH (2.60 g cm⁻³; no SiO2 metadata in "
                         "the refractive index)",
            description="volcanic ash (Etna, Deguine et al. 2020)"),
    Species("DUST", AEROSOL, aerosol_type="mineral", ri_source="sand_and_dust",
            density_rule="mieEODG DENSITY_DUST (2.60 g cm⁻³)",
            description="mineral dust (Di Biagio et al. 2014)"),
    Species("SOOT", AEROSOL, aerosol_type="carbon",
            density_rule="mieEODG DENSITY_BLACK_CARBON (1.80 g cm⁻³)",
            description="black carbon; deferred: no refractive index"),
    Species("OC", AEROSOL, aerosol_type="carbon",
            description="organic carbon (wildfire smoke); deferred: no refractive index"),
    Species("AEROSOL", AEROSOL, aerosol_type="generic",
            description="RFM molecule 100, native km⁻¹ slot; deferred"),
]

#: Every species, by name.
REGISTRY: Mapping[str, Species] = MappingProxyType(
    {s.name: s for s in [_gas(g) for g in _GASES] + _AEROSOLS})


def get(name: str) -> Species:
    """The species of ``name``; ``KeyError`` naming the known ones otherwise."""
    try:
        return REGISTRY[name]
    except KeyError:
        raise KeyError(f"unknown species {name!r} (known: {sorted(REGISTRY)})") from None


def gases(rfm_only: bool = False) -> list[str]:
    """Gas names, in registry order."""
    return [s.name for s in REGISTRY.values() if s.is_gas and (s.rfm or not rfm_only)]


def aerosols() -> list[str]:
    """Aerosol names, sorted."""
    return sorted(s.name for s in REGISTRY.values() if s.is_aerosol)


def is_aerosol(name: str) -> bool:
    """Whether ``name`` is a registered aerosol."""
    s = REGISTRY.get(name)
    return s is not None and s.is_aerosol


def component_fractions(name: str, wt: float | np.ndarray | None = None
                        ) -> dict[str, float | np.ndarray]:
    """
    Mass fraction of each molecular component of an aerosol particle.

    Parameters
    ----------
    name : str
    wt : float, array or None
        The weight fraction of a component given by a parameter (the acid of
        ``H2SO4_aq``), per spectrum where it varies.  Required for such a
        component.

    Returns
    -------
    dict
        Formula → fraction; empty for an aerosol without molecular content.

    Raises
    ------
    ValueError
        When a component needs ``wt`` and none is given.
    """
    s = get(name)
    out: dict[str, float | np.ndarray] = {}
    for c in s.components:
        if c.remainder:
            continue
        if c.parameter is not None:
            if wt is None:
                raise ValueError(f"{name}: the mass fraction of {c.formula} is the "
                                 f"parameter {c.parameter}; give wt")
            out[c.formula] = np.asarray(wt, dtype=float) if np.ndim(wt) else float(wt)
        else:
            out[c.formula] = float(c.fraction)
    rest = [c for c in s.components if c.remainder]
    if rest:
        out[rest[0].formula] = 1.0 - sum(out.values())
    return out


def component_masses(name: str, particle_mass: float | np.ndarray,
                     wt: float | np.ndarray | None = None) -> dict[str, np.ndarray]:
    """
    Mass column of each component [g m⁻²] from the particle mass column.

    Parameters
    ----------
    name : str
    particle_mass : float or array
        The retrieved aerosol mass column [g m⁻²].
    wt : float, array or None
        As for :func:`component_fractions`.
    """
    m = np.asarray(particle_mass, dtype=float)
    return {f: m * frac for f, frac in component_fractions(name, wt).items()}


def component_columns(name: str, particle_mass: float | np.ndarray,
                      wt: float | np.ndarray | None = None) -> dict[str, np.ndarray]:
    """
    Molecular column of each component [molec cm⁻²], comparable with a gas
    slant column: mass / molar mass × N_A, per cm².
    """
    return {f: m / MOLAR_MASS[f] * N_A / 1e4
            for f, m in component_masses(name, particle_mass, wt).items()}


def density(name: str, wt: float | None = None) -> float:
    """
    Particle density [g cm⁻³] of an aerosol, from mieEODG (never a copy).

    Parameters
    ----------
    name : str
    wt : float or None
        Acid weight fraction of a solution with a fitted one (``H2SO4_aq``);
        the registry's fixed fraction otherwise.

    Raises
    ------
    ValueError
        For a gas, or an aerosol whose density mieEODG does not set.
    """
    from mieEODG import constants as mc
    s = get(name)
    if not s.is_aerosol:
        raise ValueError(f"{name} is a gas")
    if s.aerosol_type == "solution":
        w = component_fractions(name, wt)[s.solute.formula]
        fn = {"H2SO4": mc.h2so4_density, "HNO3": mc.hno3_density}[s.solute.formula]
        return float(fn(w))
    table = {"WATER": mc.DENSITY_WATER, "ICE": mc.ice_density(266.0),
             "ASH": mc.DENSITY_ASH, "DUST": mc.DENSITY_DUST,
             "SOOT": mc.DENSITY_BLACK_CARBON}
    if name not in table:
        raise ValueError(f"{name}: no density rule in mieEODG")
    return float(table[name])
