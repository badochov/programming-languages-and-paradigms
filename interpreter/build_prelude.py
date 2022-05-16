#!/usr/bin/env python3

import os
import re

with os.popen("./src/Grammar/Test src/prelude/prelude.zoya") as handle:
    out = handle.read()

prog = out.split("[Abstract Syntax]")[1].split("[Linearized tree]")[0].strip()
nothing_pos_prog = re.sub(r"\(Just \(\d+\,\d+\)\)", "Nothing", prog)

template = """module ZoyaPrelude where

import Common (preprocess)
import Data.Maybe
import Grammar.Abs

prelude :: Program
prelude = %s
"""

with open("src/ZoyaPrelude.hs", "w") as f:
    f.write(template % nothing_pos_prog)