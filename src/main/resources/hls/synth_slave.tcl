
## -- Create project
open_project "slave_registers"

## -- Set top level
set_top "slave_registers"

## -- Add file
add_files @FILE@ -cflags "-std=c++11 -Wno-narrowing"

## -- Create solution
open_solution -reset solution -flow_target vitis

## -- Define Xilinx FPGA
set_part @PART@

## -- Configure Interface
config_interface -clock_enable=0 -m_axi_addr64 -m_axi_offset off -register_io off 

## -- Define clock period
create_clock -period 2 -name default

## -- Clock uncertainty
set_clock_uncertainty 27%

## -- Config RTL
config_rtl -reset_level low

## -- Config compile
# config_compile -name_max_length 512 -no_signed_zeros=0 -pipeline_loops 256 -unsafe_math_optimizations=0

## -- Run synthesis
csynth_design

exit
## EOF