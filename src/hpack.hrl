
-record(hpack_ctx, {
    table = [] :: [{pos_integer(), header_name(), header_value()}],
    cur_size = 0 :: non_neg_integer(),
    max_size = 4096 :: non_neg_integer(),
    conn_max_size = 4096 :: non_neg_integer()
}).


-define(DYNAMIC_TABLE_MIN_INDEX, 62).
-define(ERROR(Reason), throw({hpack_error, Reason})).


-type context() :: #hpack_ctx{}.
-type header_name() :: binary().
-type header_value() :: binary().
-type header_opt() :: never_index | no_index | no_name_index | uncompressed.
-type header() :: {header_name(), header_value(), [header_opt()]}.
-type headers() :: [header()].
-type index_entry() :: {pos_integer(), header_name(), header_value()}.


-type encode_error() ::
    {invalid_table_size, integer()}.


-type decode_error() ::
    {invalid_packet, binary()} |
    {invalid_index, integer()} |
    {invalid_size_update, integer()} |
    {invalid_huffman_encoding, partial_code} |
    {invalid_huffman_encoding, internal_eos}.


-type match_result() ::
    {hdr_indexed, pos_integer()} |
    {name_indexed, pos_integer()} |
    no_index_entry.


