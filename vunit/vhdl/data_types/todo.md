Here is a list of change. The proposed modifications on the `codec` package are retro-compatible.


 + Add comments.
 + Removed hard coded value
 + Replace the type 'string' used to encode by an alias named 'code_t' to ease readability and maintainbility.
 + Inverse the `function` and alias `names` to ease readability and maintainbility:
   ```vhdl
   function encode (constant code : std_ulogic_vector) return string;
   function decode (constant code : string) return std_ulogic_vector;
   alias encode_std_ulogic_vector is encode[std_ulogic_vector return string];
   alias decode_std_ulogic_vector is decode[string return std_ulogic_vector];
   ```
 + The code length for integer automatically adapts to the width of the integer (1993/2002/2008: 32bits, 2019: 64bits)
 + Functions `encode_array_header` and `get_range` are replaced (these names are not consistent with the other functions):
   - `function encode_range(range_left : integer; range_right : integer; is_ascending : boolean) return code_vec_t;`
   - `function decode_range(code : code_vec_t) return range_t;`
   - `encode_array_header` and `get_range` are deprecated but preserved for backward compatibility.
   - `encode_array_header` and `get_range` take the encoded string of `range_left`, `range_right` and `is_ascending` while `encode_range` and `decode_range` directly take integers and a boolean.
   - Note that `encode_array_header` can encode two ranges but it was never used for that purpose (anywhere in the VUnit repo). The `encode_range` can only encode a single range.
 + Replace the `ieee.numeric_std.unsigned` by `ieee.numeric_std.unresolved_unsigned`
 + Replace the `ieee.numeric_std.signed` by `ieee.numeric_std.unresolved_signed`
 + Simplify the local `log2` function used to encode the  `real` type by using the `ieee.math_real` package.
 + The type `std_ulogic_array` is similar to the `std_ulogic_vector` but with an integer range. However, it was not the case for bit_vector. There is now a type `bit_array` for that purpose.
   The same idea could be applied to `string`, but I did not see the use case so it was not implemented.
 + For each encoded type which are a `scalar`, an `enumerated type` or a `records type`, there is a constant named `code_length_type_name` which indicate how many character are needed to encode a value of the selected type.
 + For each encoded type which an `array type`, there is a function named `function code_length_type_name(length : natural) return natural;` which indicate how many character are needed to encode a value of the selected type.
 + The function `to_byte_array` and `from_byte_array` are replaced (these names are not consistent with the other functions and the introduction of `bit_array` required a change of `bit_vector` to `bit_array`):
   - `function encode_raw_bit_array(data : bit_array) return code_t;`
   - `function decode_raw_bit_array(code : code_t) return bit_array;`
   - `to_byte_array` and `from_byte_array` are deprecated but preserved for backward compatibility.


 + Move the declaration of the type `std_ulogic_array` from `codec_builder.vhd` to `types.vhd` ? TBC
 +
 + Have the `encode` and `decode` function closer to one another.
 + Move the `get_simulator_resolution` function out of `codec_builder.vhd` ? TBC and where ?


TODO checker que les noms de paramètres n'ont pas changé.
TODO Check les NULL_RANGE
