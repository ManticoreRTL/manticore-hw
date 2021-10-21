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
set path_to_packaged @PACKAGED_PATH@
set path_to_tmp_project @TEMP_KERNEL_PACKAGE_PATH@
set master_interface_0 @MASTER_INTERFACE_0@
set master_interface_1 @MASTER_INTERFACE_1@
set master_interface_2 @MASTER_INTERFACE_2@
set master_interface_3 @MASTER_INTERFACE_3@
set slave_interface @SLAVE_INTERFACE@


create_project -force kernel_pack $path_to_tmp_project 
add_files -norecurse [glob $path_to_hdl/*.v $path_to_hdl/*.sv]
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
ipx::associate_bus_interfaces -busif $master_interface_0 -clock clock $core
ipx::associate_bus_interfaces -busif $master_interface_1 -clock clock $core
ipx::associate_bus_interfaces -busif $master_interface_2 -clock clock $core
ipx::associate_bus_interfaces -busif $master_interface_3 -clock clock $core
ipx::associate_bus_interfaces -busif $slave_interface -clock clock $core

# Specify the freq_hz parameter 
set clkbif      [::ipx::get_bus_interfaces -of $core "clock"]
set clkbifparam [::ipx::add_bus_parameter -quiet "FREQ_HZ" $clkbif]
# Set desired frequency                   
set_property value 300000000 $clkbifparam
# set value_resolve_type 'user' if the frequency can vary. 
set_property value_resolve_type user $clkbifparam
# set value_resolve_type 'immediate' if the frequency cannot change. 
# set_property value_resolve_type immediate $clkbifparam


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
