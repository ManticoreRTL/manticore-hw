

# create a pblock for controller and clock management, this should take a whole
# clock region and be placed closed to the shell
# create_pblock mctrl_pb
# resize_pblock mctrl_pb -add CLOCKREGION_X2Y7:CLOCKREGION_X2Y7

# add_cells_to_pblock mctrl_pb [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/controller \
#                                               level0_i/ulp/ManticoreKernel_1/inst/clock_distribution   \
#                              ]] -clear_locs


create_pblock manticore_entry
resize_pblock manticore_entry -add CLOCKREGION_X0Y9:CLOCKREGION_X2Y9
# set_property EXCLUDE_PLACEMENT 0 [get_plocks manticore_entry]

add_cells_to_pblock manticore_entry [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/controller             \
                                                     level0_i/ulp/ManticoreKernel_1/inst/clock_distribution               \
                                                     level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_0 \
                                                     level0_i/ulp/ManticoreKernel_1/inst/s_axi_clock_crossing             \
                                                     level0_i/ulp/ManticoreKernel_1/inst/m_axi_bank_0_clock_crossing      \
                                                     level0_i/ulp/ManticoreKernel_1/inst/manticore/bootloader             \
                                                     level0_i/ulp/ManticoreKernel_1/inst/slave                            \
                                                     level0_i/ulp/ManticoreKernel_1/inst/manticore/memory_intercept       \
                                                     ]] -clear_locs
