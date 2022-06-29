#
# Copyright (C) 2019-2021 Xilinx, Inc
#
# Licensed under the Apache License, Version 2.0 (the "License"). You may
# not use this file except in compliance with the License. A copy of the
# License is located at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
#

set path_to_hdl @VERILOG_PATH@
set path_to_ips @IP_PATH@
set path_to_packaged @PACKAGED_PATH@
set path_to_tmp_project @TEMP_KERNEL_PACKAGE_PATH@
set master_interface_0 @MASTER_INTERFACE_0@
# set master_interface_1 @MASTER_INTERFACE_1@
# set master_interface_2 @MASTER_INTERFACE_2@
# set master_interface_3 @MASTER_INTERFACE_3@
set slave_interface @SLAVE_INTERFACE@


create_project -force kernel_pack $path_to_tmp_project
add_files -norecurse [glob $path_to_hdl/*.v \
                           $path_to_hdl/*.sv \
                           $path_to_ips/axi4_clock_converter/axi4_clock_converter.xci \
                           $path_to_ips/axi4lite_clock_converter/axi4lite_clock_converter.xci \
                           $path_to_ips/clk_dist/clk_dist.xci]

update_compile_order -fileset sources_1
update_compile_order -fileset sim_1
ipx::package_project -root_dir $path_to_packaged -vendor vlsc.epfl.com -library RTLKernel -taxonomy /KernelIP -import_files -set_current false
ipx::unload_core $path_to_packaged/component.xml
ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory $path_to_packaged $path_to_packaged/component.xml

set core [ipx::current_core]

set_property core_revision 2 $core
foreach up [ipx::get_user_parameters] {
  ipx::remove_user_parameter [get_property NAME $up] $core
}
ipx::associate_bus_interfaces -busif $master_interface_0 -clock ap_clk $core -reset ap_rst_n
ipx::associate_bus_interfaces -busif $slave_interface -clock ap_clk $core -reset ap_rst_n




set_property xpm_libraries {XPM_CDC XPM_MEMORY XPM_FIFO} $core
set_property sdx_kernel true $core
set_property sdx_kernel_type rtl $core
set_property supported_families { } $core
set_property auto_family_support_level level_2 $core
ipx::create_xgui_files $core
ipx::update_checksums $core
ipx::check_integrity -kernel $core
ipx::save_core $core
close_project -delete
