

#include <ap_int.h>
#include <cstdint>


using half_word_t = uint16_t;
using address_t = uint64_t;

uint16_t ReadSingleWord(uint16_t *memory_pointer) {
#pragma HLS INTERFACE mode = m_axi depth = 1 max_read_burst_length =           \
    1 max_widen_bitwidth = 32 max_write_burst_length =                         \
        1 num_read_outstanding = 1 num_write_outstanding = 1 port =            \
            memory_pointer offset = direct
#pragma HLS INTERFACE ap_ctrl_hs register port = return
  return memory_pointer[0];
}
