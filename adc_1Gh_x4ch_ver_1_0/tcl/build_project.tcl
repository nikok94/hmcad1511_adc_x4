########################################################################
#
#
#

set root_dir [ file normalize [file dirname [info script]]/../ ]
set device "xc6slx16ftg256-2"
set prj_name "hmcad1511_adc_x4"
set language "VHDL"

# Create project
create_project $prj_name -force $root_dir/prj

# Set the directory path for the new project
set proj_dir [get_property directory [current_project]]

# Set project properties
set obj [get_projects $prj_name]
set_property "part" $device $obj
set_property "target_language" "VHDL" $obj

########################################################################
# Sources
add_files -norecurse ../src/hmcad_x4_top.vhd
add_files -norecurse ../src/clock_generator.vhd
add_files -norecurse ../src/SPI/spi_master.vhd
add_files -norecurse ../src/SPI/spi_data_receiver.vhd
add_files -norecurse ../src/SPI/spi_data_transceiver.vhd
add_files -norecurse ../src/HMCAD1511_v3_00/HMCAD1511_v3_00.vhd
add_files -norecurse ../src/HMCAD1511_v3_00/data_deserializer.vhd
add_files -norecurse ../src/HMCAD1511_v3_00/high_speed_clock_to_serdes.vhd
add_files -norecurse ../src/SPIFI/spifi_module.vhd
add_files -norecurse ../src/hmcad_x4_block.vhd
add_files -norecurse ../src/data_recorder.vhd
add_files -norecurse ../src/trigger_capture.vhd
add_files -norecurse ../src/true_dpram_sclk.vhd
add_files -norecurse ../src/serdes_1_to_n_clk_ddr_s8_diff.v
add_files -norecurse ../src/serdes_1_to_n_data_ddr_s8_diff.v


########################################################################
# UCF
add_files -fileset [current_fileset -constrset] -norecurse ../ucf/hmcad_x4_constr.ucf
set_property target_constrs_file ../ucf/hmcad_x4_constr.ucf [current_fileset -constrset]


#set_property SOURCE_SET sources_1 [get_filesets sim_1]
#add_files -fileset sim_1 -norecurse -scan_for_includes $root_dir/sim/const_package.vhd
#add_files -fileset sim_1 -norecurse -scan_for_includes $root_dir/sim/pci_arbt_module.vhd
#add_files -fileset sim_1 -norecurse -scan_for_includes $root_dir/sim/pci_host_module.vhd
#add_files -fileset sim_1 -norecurse -scan_for_includes $root_dir/sim/stream_pci_TB.vhd
#add_files -fileset sim_1 -norecurse -scan_for_includes $root_dir/sim/host_pc_module.vhd
#
#update_compile_order -fileset sim_1

