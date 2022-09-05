

create_pblock mantictrl
resize_pblock mantictrl -add CLOCKREGION_X2Y7
add_cells_to_pblock mantictrl [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/clock_distribution \
                                               level0_i/ulp/ManticoreKernel_1/inst/manticore/controller \
                                               level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_0 \
                                               level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_0 \
                                               level0_i/ulp/ManticoreKernel_1/inst/axi_cache \
                                        ]]

set_property CLOCK_DELAY_GROUP MantictoreClk [get_nets {level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/wiz/inst/clk_out1 \
                                                        level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/clock_distribution_compute_clock}]
set_property USER_CLOCK_ROOT X2Y7 [get_nets {level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/wiz/inst/clk_out1 \
                                              level0_i/ulp/ManticoreKernel_1/inst/clock_distribution/clock_distribution_compute_clock}]







# create_pblock mpb00
# resize_pblock mpb00 -add CLOCKREGION_X0Y10:CLOCKREGION_X1Y10

# add_cells_to_pblock mpb00 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_0 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_0 \
#                                      ]]



# create_pblock mpb10
# resize_pblock mpb10 -add CLOCKREGION_X2Y10

# add_cells_to_pblock mpb10 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_0 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_0 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_0 \
#                                     ]]


# create_pblock mpb20
# resize_pblock mpb20 -add CLOCKREGION_X3Y10

# add_cells_to_pblock mpb20 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_0 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_0 \
#                                     ]]


# create_pblock mpb30
# resize_pblock mpb30 -add CLOCKREGION_X4Y10:X5Y10

# add_cells_to_pblock mpb30 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_1 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_0 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_0 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_0 \
#                                     ]]


# create_pblock mpb01
# resize_pblock mpb01 -add CLOCKREGION_X0Y11:X1Y11

# add_cells_to_pblock mpb01 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_9 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_9 \
#                                     ]]



# create_pblock mpb11
# resize_pblock mpb11 -add CLOCKREGION_X2Y11

# add_cells_to_pblock mpb11 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_9 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_9 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_9 \
#                                     ]]


# create_pblock mpb21
# resize_pblock mpb21 -add CLOCKREGION_X3Y11

# add_cells_to_pblock mpb31 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_9 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_9 \
#                                     ]]



# create_pblock mpb31
# resize_pblock mpb31 -add CLOCKREGION_X4Y11:CLOCKREGION_X5Y11

# add_cells_to_pblock mpb31 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_2 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_9 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_9 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_9 \
#                                     ]]



# create_pblock mpb02
# resize_pblock mpb02 -add CLOCKREGION_X0Y12:CLOCKREGION_X1Y12

# add_cells_to_pblock mpb02 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_8 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_8 \
#                                     ]]


# create_pblock mpb12
# resize_pblock mpb12 -add CLOCKREGION_X2Y12

# add_cells_to_pblock mpb12 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_8 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_8 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_8 \
#                                     ]]


# create_pblock mpb22
# resize_pblock mpb22 -add CLOCKREGION_X3Y12

# add_cells_to_pblock mpb22 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_8 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_8 \
#                                     ]]

# create_pblock mpb32
# resize_pblock mpb32 -add CLOCKREGION_X4Y12:CLOCKREGION_X5Y12

# add_cells_to_pblock mpb32 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_3 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_8 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_8 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_8 \
#                                     ]]


# create_pblock mpb03
# resize_pblock mpb03 -add CLOCKREGION_X0Y13:CLOCKREGION_X1Y13

# add_cells_to_pblock mpb03 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_7 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_7 \
#                                     ]]


# create_pblock mpb13
# resize_pblock mpb13 -add CLOCKREGION_X2Y13

# add_cells_to_pblock mpb13 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_7 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_7 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_7 \
#                                     ]]



# create_pblock mpb23
# resize_pblock mpb23 -add CLOCKREGION_X3Y13

# add_cells_to_pblock mpb23 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_7 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_7 \
#                                     ]]


# create_pblock mpb33
# resize_pblock mpb33 -add CLOCKREGION_X4Y13:CLOCKREGION_X5Y13

# add_cells_to_pblock mpb33 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_4 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_7 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_7 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_7 \
#                                     ]]



# create_pblock mpb04
# resize_pblock mpb04 -add CLOCKREGION_X0Y14:CLOCKREGION_X1Y14

# add_cells_to_pblock mpb04 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_9_6 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_8_6 \
#                                     ]]


# create_pblock mpb14
# resize_pblock mpb14 -add CLOCKREGION_X2Y14

# add_cells_to_pblock mpb14 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_0_6 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_1_6 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_7_6 \
#                                     ]]



# create_pblock mpb24
# resize_pblock mpb24 -add CLOCKREGION_X3Y14

# add_cells_to_pblock mpb24 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_2_6 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_6_6 \
#                                     ]]


# create_pblock mpb34
# resize_pblock mpb34 -add CLOCKREGION_X4Y14:CLOCKREGION_X5Y14

# add_cells_to_pblock mpb33 [get_cells [list level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_5 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_3_6 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_4_6 \
#                                            level0_i/ulp/ManticoreKernel_1/inst/manticore/compute_array/core_5_6 \
#                                     ]]
