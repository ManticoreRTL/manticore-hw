

#include <ap_int.h>
#include <cstdint>
using CacheLine = ap_uint<256>;
using Address = uint64_t;

enum CacheBackendCommand { Read = 0, Write = 1, WriteBack = 2 };
CacheLine @NAME@(CacheLine *memory_pointer, Address raddr,
                         Address waddr, uint8_t cmd, CacheLine wline) {
#pragma HLS INTERFACE mode = m_axi depth = 1 max_read_burst_length =           \
    1 max_widen_bitwidth = 32 max_write_burst_length =                         \
        1 num_read_outstanding = 1 num_write_outstanding = 1 port =            \
            memory_pointer offset = direct 
#pragma HLS INTERFACE ap_ctrl_hs register port = return

  CacheLine rline = 0;

  switch (cmd) {
  case Read:
    rline = memory_pointer[raddr];
    break;
  case Write:
    memory_pointer[waddr] = wline;
    break;
  case WriteBack:
    memory_pointer[waddr] = wline;
    rline = memory_pointer[raddr];
    break;
  }
  return rline;
}
