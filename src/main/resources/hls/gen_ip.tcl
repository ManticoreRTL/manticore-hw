#
# Copyright 2021 Xilinx, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# set the device part from command line argvs
# set_part [lindex $argv 0]
set ip_location  [lindex $argv 1]
set request_freq [lindex $argv 2]
set cacheline_width [lindex $argv 3]
puts "Requested freqenecy : $request_freq"
set_part xcu200-fsgd2104-2-e
set freq_hz [expr $request_freq * 1000000]
puts "Freq HZ : $freq_hz"
# ----------------------------------------------------------------------------
# create clock gen IP
# ----------------------------------------------------------------------------
create_ip -name clk_wiz \
          -vendor xilinx.com \
          -library ip \
          -version 6.0 \
          -module_name clk_dist \
          -dir $ip_location

set_property -dict [list CONFIG.OPTIMIZE_CLOCKING_STRUCTURE_EN {true}    \
                         CONFIG.PRIM_IN_FREQ {300.00}                    \
                         CONFIG.CLKOUT2_USED {true}                      \
                         CONFIG.CLKOUT1_REQUESTED_OUT_FREQ $request_freq \
                         CONFIG.CLKOUT2_REQUESTED_OUT_FREQ $request_freq \
                         CONFIG.CLKOUT1_DRIVES {Buffer}                  \
                         CONFIG.CLKOUT2_DRIVES {Buffer_with_CE}          \
                         CONFIG.CLKOUT1_MATCHED_ROUTING {true}           \
                         CONFIG.CLKOUT2_MATCHED_ROUTING {true}        ]  \
                [get_ips clk_dist]

# generate_target all [get_files  ./ip_generation/clk_dist/clk_dist.xci]


# ----------------------------------------------------------------------------
# create axi clock converter IP
# ----------------------------------------------------------------------------
create_ip -name axi_clock_converter \
          -vendor xilinx.com \
          -library ip \
          -version 2.1 \
          -module_name axi4lite_clock_converter \
          -dir $ip_location

set_property -dict [list CONFIG.PROTOCOL {AXI4LITE} \
                         CONFIG.ADDR_WIDTH {12} \
                         CONFIG.SYNCHRONIZATION_STAGES {3} \
                         CONFIG.DATA_WIDTH {32} \
                         CONFIG.ID_WIDTH {0} \
                         CONFIG.AWUSER_WIDTH {0} \
                         CONFIG.ARUSER_WIDTH {0} \
                         CONFIG.RUSER_WIDTH {0} \
                         CONFIG.WUSER_WIDTH {0} \
                         CONFIG.BUSER_WIDTH {0} \
                         CONFIG.ACLK_ASYNC {1} \
                         CONFIG.SI_CLK.FREQ_HZ {300000000} \
                         CONFIG.MI_CLK.FREQ_HZ $freq_hz \
                         ] \
             [get_ips axi4lite_clock_converter]

generate_target all [get_files $ip_location/axi4lite_clock_converter/axi4lite_clock_converter.xci]

create_ip -name axi_clock_converter \
          -vendor xilinx.com \
          -library ip \
          -version 2.1 \
          -module_name axi4_clock_converter \
          -dir $ip_location

set_property -dict [list CONFIG.PROTOCOL {AXI4} \
                         CONFIG.ADDR_WIDTH {64} \
                         CONFIG.SYNCHRONIZATION_STAGES {3} \
                         CONFIG.DATA_WIDTH ${cacheline_width} \
                         CONFIG.ID_WIDTH {0} \
                         CONFIG.AWUSER_WIDTH {0} \
                         CONFIG.ARUSER_WIDTH {0} \
                         CONFIG.RUSER_WIDTH {0} \
                         CONFIG.WUSER_WIDTH {0} \
                         CONFIG.BUSER_WIDTH {0} \
                         CONFIG.ACLK_ASYNC {1} ] \
             [get_ips axi4_clock_converter]

generate_target all [get_files $ip_location/axi4_clock_converter/axi4_clock_converter.xci]
