-- This package contains support functions for standard codec building
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2022, Lars Asplund lars.anders.asplund@gmail.com

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_complex.all;
use ieee.numeric_bit.all;
use ieee.numeric_std.all;

use std.textio.all;


-------------------------------------------------------------------------------
-- Package declaration
-------------------------------------------------------------------------------
package common_pkg is

  -----------------------------------------------------------------------------
  -- Functions
  -----------------------------------------------------------------------------

  -- Returns the ceil value of the division
  function ceil_div(dividend : natural; divisor : natural) return natural; --TODO (extend to integer ?)

  -- Retrieve the time resolution of the simulator
  function get_simulator_resolution return time;

  -- Retrieve the integer width of the simulator
  function get_simulator_integer_width return positive;


  -----------------------------------------------------------------------------
  -- Constant
  -----------------------------------------------------------------------------

  -- Bytes <-> bit conversion
  constant BB : positive := 8; -- TBC useful ?

  -- Time resolution of the simulator used
  constant SIMULATOR_RESOLUTION : time;

  -- Time resolution of the simulator used
  constant SIMULATOR_INTEGER_WIDTH : positive;

  -- Time resolution of the simulator used
  constant SIMULATOR_TIME_WIDTH : positive := 64; -- TODO assumed value

end package;


-------------------------------------------------------------------------------
-- Package body
-------------------------------------------------------------------------------
package body common_pkg is

  -----------------------------------------------------------------------------
  -- Functions
  -----------------------------------------------------------------------------

  function ceil_div(dividend : natural; divisor : natural) return natural is
  begin
    return (dividend + divisor - 1) / divisor;
  end function;


  function get_simulator_resolution return time is
    type time_array_t is array (integer range <>) of time;
    variable resolution : time;
    -- Note it is important to fully constraint the constant to
    -- insure that the loop go through the array in the wanted order
    constant RESOLUTIONS : time_array_t(1 to 8) := (
      1.0e-15 sec, 1.0e-12 sec , 1.0e-9 sec, 1.0e-6 sec, 1.0e-3 sec, 1 sec, 1 min, 1 hr
    );
  begin
    for r in RESOLUTIONS'range loop
      resolution := RESOLUTIONS(r);
      exit when resolution > 0 sec;
    end loop;
    return resolution;
  end function;


  function get_simulator_integer_width return positive is -- TBC Is this function written well enough ?
  begin
    if integer'high = 2_147_483_647 then
      return 32;
    else
      return 64;
    end if;
  end function;


  -----------------------------------------------------------------------------
  -- Constant
  -----------------------------------------------------------------------------

  constant SIMULATOR_RESOLUTION : time := get_simulator_resolution;
  constant SIMULATOR_INTEGER_WIDTH : positive := get_simulator_integer_width;

end package body;
