
#include <cstdint>
#define NO_DCE(var)                                                            \
  { auto val = reinterpret_cast<volatile uint8_t &>(var); }

void slave_registers(uint64_t h_inst_base, uint64_t h_vcd_sym_tab_base,
                     uint64_t h_vcd_log_base, uint32_t h_sched_len,
                     uint8_t h_clear_exception, uint64_t d_vcycles,
                     uint32_t d_bootcycles, uint16_t d_except_id_0,
                     uint16_t d_except_id_1, uint16_t d_except_id_2,
                     uint16_t d_except_id_3, uint32_t d_status) {
/**
h_inst_base
h_vcd_sym_tab_base
h_vcd_log_base
h_sched_len
d_bootcycles
d_except_id_0
d_except_id_1
d_except_id_2
d_except_id_3
d_status
*/
#pragma HLS interface s_axilite port = h_inst_base bundle = control
#pragma HLS interface s_axilite port = h_vcd_sym_tab_base bundle = control
#pragma HLS interface s_axilite port = h_vcd_log_base bundle = control
#pragma HLS interface s_axilite port = h_sched_len bundle = control
#pragma HLS interface s_axilite port = d_bootcycles bundle = control
#pragma HLS interface s_axilite port = h_clear_exception bundle = control
#pragma HLS interface s_axilite port = d_vcycles bundle = control
#pragma HLS interface s_axilite port = d_except_id_0 bundle = control
#pragma HLS interface s_axilite port = d_except_id_1 bundle = control
#pragma HLS interface s_axilite port = d_except_id_2 bundle = control
#pragma HLS interface s_axilite port = d_except_id_3 bundle = control
#pragma HLS interface s_axilite port = d_status bundle = control
#pragma HLS interface s_axilite port = return bundle = control

  NO_DCE(h_inst_base)
  NO_DCE(h_vcd_sym_tab_base)
  NO_DCE(h_vcd_log_base)
  NO_DCE(h_sched_len)
  NO_DCE(h_clear_exception)
  NO_DCE(d_vcycles)
  NO_DCE(d_bootcycles)
  NO_DCE(d_except_id_0)
  NO_DCE(d_except_id_1)
  NO_DCE(d_except_id_2)
  NO_DCE(d_except_id_3)
  NO_DCE(d_status)
}
