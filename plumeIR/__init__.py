"""
plumeIR v3 — Open Path FTIR spectral retrieval package.

Logging
-------
The library only creates loggers (``logging.getLogger(__name__)``, all under
the ``'plumeIR'`` hierarchy) and attaches a ``NullHandler``.  It never adds a
console or file handler: the client does.  The command line attaches a console
handler and a log file in the run's output directory; a GUI attaches its own.
A script that wants to see progress can call, for example::

    logging.basicConfig(level=logging.INFO)
"""

from __future__ import annotations

import logging

__version__ = "3.0.0rc3"
__author__ = "J.-F. Smekens"

logging.getLogger("plumeIR").addHandler(logging.NullHandler())
