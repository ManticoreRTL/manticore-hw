

create_pblock mantictrl
resize_pblock mantictrl -add CLOCKREGION_X2Y10
add_cells_to_pblock mantictrl [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/clock_distribution \
                                               level0_i/ulp/ManticoreKernel_1/inst/manticore/controller \
                                               level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_0 \
                                               level0_i/ulp/ManticoreKernel_1/inst/axi_cache \
                                        ]]

# set_property CLOCK_DELAY_GROUP MantictoreClk [get_nets {level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/wiz/inst/clk_out1 \
#                                                         level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/clock_distribution_compute_clock}]
set_property USER_CLOCK_ROOT X2Y10 [get_nets {level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/wiz/inst/clk_out1 \
                                              level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/clock_distribution_compute_clock}]






