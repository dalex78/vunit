
 + Move the `val_t`, `vec_t`, `vav_t`, etc alias from `string_ptr_pkg.vhd` to `types.vhd`
 + Move the declaration of the type `std_ulogic_array` from `codec_builder.vhd` to `types.vhd` ? TBC
 + Move the `get_simulator_resolution` function out of `codec_builder.vhd` ? TBC and where ?
 + Have the `encode` and `decode` function close to one another.
 + Inverse the `function` and alias `names`:
   ```vhdl
   function encode (constant code : std_ulogic_vector) return string;
   function decode (constant code : string) return std_ulogic_vector;
   alias encode_std_ulogic_vector is encode[std_ulogic_vector return string];
   alias decode_std_ulogic_vector is decode[string return std_ulogic_vector];
   ```
 + Make so that when the integer type is other than 32 bits (64 bits in VHDL 2019), the code length for integer automatically adapts.
 + Rename `encode_array_header` into `encode_range`
 + Rename `get_range` into `decode_range`
 + Make `encode_array_header` so that it accept standard type instead of string.
 + Remove the ability to encode a second range in `encode_array_header`: never used (TBC)
 + (Note that the `get_range` function do not enable t retrieve the second range encoded by `encode_array_header`)
 + Replace the `ieee.numeric_std.unsigned` by `ieee.numeric_std.unresolved_unsigned`
 + Replace the `ieee.numeric_std.signed` by `ieee.numeric_std.unresolved_signed`
