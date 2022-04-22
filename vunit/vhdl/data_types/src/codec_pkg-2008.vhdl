-- This file provides functionality to encode/decode standard types to/from string.
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,
-- You can obtain one at http://mozilla.org/MPL/2.0/.
--
-- Copyright (c) 2014-2022, Lars Asplund lars.anders.asplund@gmail.com


library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_complex.all;
use ieee.numeric_bit.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use ieee.float_pkg.all;

library work;
use work.codec_pkg.all;


-------------------------------------------------------------------------------
-- Package declaration
-------------------------------------------------------------------------------
package codec_pkg_2008 is

  -- This package extends the codec_pkg to support the types
  -- introduced by the VHDL-2008 revision.
  -- The main documentation of the coded functionnality are located
  -- on the codec_pkg.vhd file.


  --===========================================================================
  -- API for the CASUAL USERS
  --===========================================================================

  function encode_boolean_vector(data : boolean_vector) return code_t;
  function decode_boolean_vector(code : string) return boolean_vector;
  alias encode is encode_boolean_vector[boolean_vector return code_t];
  alias decode is decode_boolean_vector[string return boolean_vector];

  function encode_integer_vector(data : integer_vector) return code_t;
  function decode_integer_vector(code : string) return integer_vector;
  alias encode is encode_integer_vector[integer_vector return code_t];
  alias decode is decode_integer_vector[string return integer_vector];

  function encode_real_vector(data : real_vector) return code_t;
  function decode_real_vector(code : string) return real_vector;
  alias encode is encode_real_vector[real_vector return code_t];
  alias decode is decode_real_vector[string return real_vector];

  function encode_time_vector(data : time_vector) return code_t;
  function decode_time_vector(code : string) return time_vector;
  alias encode is encode_time_vector[time_vector return code_t];
  alias decode is decode_time_vector[string return time_vector];

  function encode_ufixed(data : ufixed) return code_t;
  function decode_ufixed(code : string) return ufixed;
  alias encode is encode_ufixed[ufixed return code_t];
  alias decode is decode_ufixed[string return ufixed];

  function encode_sfixed(data : sfixed) return code_t;
  function decode_sfixed(code : string) return sfixed;
  alias encode is encode_sfixed[sfixed return code_t];
  alias decode is decode_sfixed[string return sfixed];

  function encode_float(data : float) return code_t;
  function decode_float(code : string) return float;
  alias encode is encode_float[float return code_t];
  alias decode is decode_float[string return float];



  --===========================================================================
  -- API for the ADVANCED USERS
  --===========================================================================

  -----------------------------------------------------------------------------
  -- Encoding length for each types
  -----------------------------------------------------------------------------
  -- If you need to retrieve the length of the encoded data whitout,
  -- encoding it, you can use these functions:

  -- These functions give you the length of the encoded array depending on the
  -- length of the array to encode
  function code_length_boolean_vector(length : natural) return natural;
  function code_length_integer_vector(length : natural) return natural;
  function code_length_real_vector(length : natural) return natural;
  function code_length_time_vector(length : natural) return natural;
  function code_length_ufixed(length : natural) return natural;
  function code_length_sfixed(length : natural) return natural;
  function code_length_float(length : natural) return natural;



  --===========================================================================
  -- API for the VUnit DEVELOPERS
  --===========================================================================

  procedure encode_boolean_vector(constant data : in boolean_vector; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_boolean_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out boolean_vector);
  alias encode is encode_boolean_vector[boolean_vector, code_index_t, code_t];
  alias decode is decode_boolean_vector[code_t, code_index_t, boolean_vector];

  procedure encode_integer_vector(constant data : in integer_vector; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_integer_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out integer_vector);
  alias encode is encode_integer_vector[integer_vector, code_index_t, code_t];
  alias decode is decode_integer_vector[code_t, code_index_t, integer_vector];

  procedure encode_real_vector(constant data : in real_vector; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_real_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out real_vector);
  alias encode is encode_real_vector[real_vector, code_index_t, code_t];
  alias decode is decode_real_vector[code_t, code_index_t, real_vector];

  procedure encode_time_vector(constant data : in time_vector; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_time_vector(constant code : in code_t; variable index : inout code_index_t; variable result : out time_vector);
  alias encode is encode_time_vector[time_vector, code_index_t, code_t];
  alias decode is decode_time_vector[code_t, code_index_t, time_vector];

  procedure encode_ufixed(constant data : in ufixed; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_ufixed(constant code : in code_t; variable index : inout code_index_t; variable result : out ufixed);
  alias encode is encode_ufixed[ufixed, code_index_t, code_t];
  alias decode is decode_ufixed[code_t, code_index_t, ufixed];

  procedure encode_sfixed(constant data : in sfixed; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_sfixed(constant code : in code_t; variable index : inout code_index_t; variable result : out sfixed);
  alias encode is encode_sfixed[sfixed, code_index_t, code_t];
  alias decode is decode_sfixed[code_t, code_index_t, sfixed];

  procedure encode_float(constant data : in float; variable index : inout code_index_t; variable code : out code_t);
  procedure decode_float(constant code : in code_t; variable index : inout code_index_t; variable result : out float);
  alias encode is encode_float[float, code_index_t, code_t];
  alias decode is decode_float[code_t, code_index_t, float];

end package;
