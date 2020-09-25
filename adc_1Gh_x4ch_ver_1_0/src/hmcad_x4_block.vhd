----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.12.2019 10:05:15
-- Design Name: 
-- Module Name: hmcad_x4_block - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.math_real.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.HMCAD1511_v3_00;
use work.spifi_module;
use work.trigger_capture;
use work.data_recorder;

entity hmcad_x4_block is
  generic (
    c_max_num_data          : integer := 128
  );
  Port (
    areset                  : in std_logic;
    acfg_bits               : in std_logic_vector(15 downto 0);
    aext_trig               : in std_logic;

    trig_start              : in std_logic;
    trig_position           : in std_logic_vector(15 downto 0);

    adc0_lclk_p             : in std_logic;
    adc0_lclk_n             : in std_logic;
    adc0_fclk_p             : in std_logic;
    adc0_fclk_n             : in std_logic;
    adc0_dx_a_p             : in std_logic_vector(3 downto 0);
    adc0_dx_a_n             : in std_logic_vector(3 downto 0);
    adc0_dx_b_p             : in std_logic_vector(3 downto 0);
    adc0_dx_b_n             : in std_logic_vector(3 downto 0);

    adc1_lclk_p             : in std_logic;
    adc1_lclk_n             : in std_logic;
    adc1_fclk_p             : in std_logic;
    adc1_fclk_n             : in std_logic;
    adc1_dx_a_p             : in std_logic_vector(3 downto 0);
    adc1_dx_a_n             : in std_logic_vector(3 downto 0);
    adc1_dx_b_p             : in std_logic_vector(3 downto 0);
    adc1_dx_b_n             : in std_logic_vector(3 downto 0);

    adc2_lclk_p             : in std_logic;
    adc2_lclk_n             : in std_logic;
    adc2_fclk_p             : in std_logic;
    adc2_fclk_n             : in std_logic;
    adc2_dx_a_p             : in std_logic_vector(3 downto 0);
    adc2_dx_a_n             : in std_logic_vector(3 downto 0);
    adc2_dx_b_p             : in std_logic_vector(3 downto 0);
    adc2_dx_b_n             : in std_logic_vector(3 downto 0);

    adc3_lclk_p             : in std_logic;
    adc3_lclk_n             : in std_logic;
    adc3_fclk_p             : in std_logic;
    adc3_fclk_n             : in std_logic;
    adc3_dx_a_p             : in std_logic_vector(3 downto 0);
    adc3_dx_a_n             : in std_logic_vector(3 downto 0);
    adc3_dx_b_p             : in std_logic_vector(3 downto 0);
    adc3_dx_b_n             : in std_logic_vector(3 downto 0);

    adcx_calib_done         : out std_logic_vector(3 downto 0);
    adcx_data_valid         : out std_logic_vector(3 downto 0);

    spifi_cs                : in std_logic;
    spifi_sck               : in std_logic;
    spifi_sio               : inout std_logic_vector(3 downto 0)

    );
end hmcad_x4_block;

architecture Behavioral of hmcad_x4_block is
  constant C_BURST_WIDTH_SPIFI        : integer := 16;
  constant num_data                   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '0');
  constant c_data_width               : integer := 64;

  signal adc0_aclk_out                : std_logic;
  signal adc0_valid                   : std_logic;
  signal adc0_data                    : std_logic_vector(63 downto 0);
  signal rec0_m_data                  : std_logic_vector(c_data_width - 1 downto 0);
  signal rec0_m_valid                 : std_logic;
  signal rec0_m_ready                 : std_logic;
  signal rec0_rst                     : std_logic;
  signal rec0_rst_sync_vec            : std_logic_vector(2 downto 0);
  signal rec0_data_valid              : std_logic;
  signal rec0_ready_dvec              : std_logic_vector(1 downto 0);


  signal adc1_aclk_out                : std_logic;
  signal adc1_valid                   : std_logic;
  signal adc1_data                    : std_logic_vector(63 downto 0);
  signal rec1_m_data                  : std_logic_vector(c_data_width - 1 downto 0);
  signal rec1_m_valid                 : std_logic;
  signal rec1_m_ready                 : std_logic;
  signal rec1_rst                     : std_logic;
  signal rec1_rst_sync_vec            : std_logic_vector(2 downto 0);
  signal rec1_data_valid              : std_logic;
  signal rec1_ready_dvec              : std_logic_vector(1 downto 0);

  signal adc2_aclk_out                : std_logic;
  signal adc2_valid                   : std_logic;
  signal adc2_data                    : std_logic_vector(63 downto 0);
  signal rec2_m_data                  : std_logic_vector(c_data_width - 1 downto 0);
  signal rec2_m_valid                 : std_logic;
  signal rec2_m_ready                 : std_logic;
  signal rec2_rst                     : std_logic;
  signal rec2_rst_sync_vec            : std_logic_vector(2 downto 0);
  signal rec2_data_valid              : std_logic;
  signal rec2_ready_dvec              : std_logic_vector(1 downto 0);

  signal adc3_aclk_out                : std_logic;
  signal adc3_valid                   : std_logic;
  signal adc3_data                    : std_logic_vector(63 downto 0);
  signal rec3_m_data                  : std_logic_vector(c_data_width - 1 downto 0);
  signal rec3_m_valid                 : std_logic;
  signal rec3_m_ready                 : std_logic;
  signal rec3_rst                     : std_logic;
  signal rec3_rst_sync_vec            : std_logic_vector(2 downto 0);
  signal rec3_data_valid              : std_logic;
  signal rec3_ready_dvec              : std_logic_vector(1 downto 0);

  signal reg_address_int              : integer;
  
  signal mux_data                     : std_logic_vector(63 downto 0);
  signal mux_data_selector            : std_logic_vector(1 downto 0);
  
  signal spifi_T                      : std_logic;
  signal spifi_cmd_counter            : std_logic;
  signal spifi_switch_byte            : std_logic_vector(7 downto 0);
  
  signal spifi_cmd_byte               : std_logic_vector(7 downto 0);
  signal spifi_cmd_valid              : std_logic;
  
  signal PCS_I                        : std_logic_vector(3 downto 0);
  signal PCS_O                        : std_logic_vector(3 downto 0);
  signal spifi_s_data                 : std_logic_vector(C_BURST_WIDTH_SPIFI*4 - 1 downto 0);
  signal spifi_s_valid                : std_logic;
  signal spifi_s_ready                : std_logic;
  signal trig_clk                     : std_logic;
  signal trigger_start                : std_logic;
  signal capture_mode                 : std_logic_vector(1 downto 0);
  signal front_condition              : std_logic_vector(1 downto 0);
  signal capture_level                : std_logic_vector(7 downto 0);
  signal start_offset                 : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

begin

mux_data_selector <= acfg_bits(4 downto 3);

mux_data_process :
  process (mux_data_selector, adc0_data, adc1_data, adc2_data, adc3_data, adc0_aclk_out, adc1_aclk_out, adc2_aclk_out, adc3_aclk_out)
  begin
     case mux_data_selector is
        when "00" => mux_data <= adc0_data;
                     trig_clk <= adc0_aclk_out;
        when "01" => mux_data <= adc1_data;
                     trig_clk <= adc1_aclk_out;
        when "10" => mux_data <= adc2_data;
                     trig_clk <= adc2_aclk_out;
        when "11" => mux_data <= adc3_data;
                     trig_clk <= adc3_aclk_out;
        when others => mux_data <= adc0_data;
                       trig_clk <= adc0_aclk_out;
     end case;
  end process;

trigger_capture_inst : entity trigger_capture
    generic map(
      c_data_width    => 64
    )
    port map( 
      clk               => trig_clk,
      rst               => areset, 

      capture_mode      => capture_mode,
      front_condition   => front_condition,

      capture_level     => capture_level,

      trigger_set_up    => trig_start,
      data              => mux_data,
      vector_valid      => open,
      ext_trig          => aext_trig,
      
      l_up              => open,
      l_down            => open,
      
      trigger_start     => trigger_start
    );

adc0_data_receiver : entity HMCAD1511_v3_00
    generic map(
      DIFF_TERM             => true
    )
    port map(
      LCLKp                 => adc0_lclk_p,
      LCLKn                 => adc0_lclk_n,
      
      FCLKp                 => adc0_fclk_p,
      FCLKn                 => adc0_fclk_n,

      DxXAp                 => adc0_dx_a_p,
      DxXAn                 => adc0_dx_a_n,
      DxXBp                 => adc0_dx_b_p,
      DxXBn                 => adc0_dx_b_n,

      reset                 => areset,
      m_aclk_out            => adc0_aclk_out,
      m_strm_valid          => adc0_valid,
      m_strm_data           => adc0_data--,

--      frame                 => open
    );

adc1_data_receiver : entity HMCAD1511_v3_00
    generic map(
      DIFF_TERM             => true
    )
    port map(
      LCLKp                 => adc1_lclk_p,
      LCLKn                 => adc1_lclk_n,
      
      FCLKp                 => adc1_fclk_p,
      FCLKn                 => adc1_fclk_n,

      DxXAp                 => adc1_dx_a_p,
      DxXAn                 => adc1_dx_a_n,
      DxXBp                 => adc1_dx_b_p,
      DxXBn                 => adc1_dx_b_n,


      reset                 => areset,
      m_aclk_out            => adc1_aclk_out,
      m_strm_valid          => adc1_valid,
      m_strm_data           => adc1_data--,

--      frame                 => open
    );

adc2_data_receiver : entity HMCAD1511_v3_00
    generic map(
      DIFF_TERM             => true
    )
    port map(
      LCLKp                 => adc2_lclk_p,
      LCLKn                 => adc2_lclk_n,
      
      FCLKp                 => adc2_fclk_p,
      FCLKn                 => adc2_fclk_n,

      DxXAp                 => adc2_dx_a_p,
      DxXAn                 => adc2_dx_a_n,
      DxXBp                 => adc2_dx_b_p,
      DxXBn                 => adc2_dx_b_n,

      reset                 => areset,
      m_aclk_out            => adc2_aclk_out,
      m_strm_valid          => adc2_valid,
      m_strm_data           => adc2_data--,

--      frame                 => open
    );

adc3_data_receiver : entity HMCAD1511_v3_00
    generic map(
      DIFF_TERM             => true
    )
    port map(
      LCLKp                 => adc3_lclk_p,
      LCLKn                 => adc3_lclk_n,

      FCLKp                 => adc3_fclk_p,
      FCLKn                 => adc3_fclk_n,

      DxXAp                 => adc3_dx_a_p,
      DxXAn                 => adc3_dx_a_n,
      DxXBp                 => adc3_dx_b_p,
      DxXBn                 => adc3_dx_b_n,

      reset                 => areset,
      m_aclk_out            => adc3_aclk_out,
      m_strm_valid          => adc3_valid,
      m_strm_data           => adc3_data--,

--      frame                 => open
    );

rec0_rst_proc : process(areset, adc0_aclk_out)
begin
  if (areset = '1') then
    rec0_rst <= '1';
    rec0_data_valid <= '0';
  elsif rising_edge(adc0_aclk_out) then
    if (spifi_switch_byte = x"00") then
      rec0_rst_sync_vec(rec0_rst_sync_vec'length - 1 downto 1) <= rec0_rst_sync_vec(rec0_rst_sync_vec'length - 2 downto 0);
      rec0_rst_sync_vec(0) <= spifi_cs;
      
    if (spifi_cs = '1') then
      rec0_ready_dvec <= (others => '0');
    else
      rec0_ready_dvec(rec0_ready_dvec'length - 1 downto 1) <= rec0_ready_dvec(rec0_ready_dvec'length - 2 downto 0);
      rec0_ready_dvec(0) <= spifi_s_ready;
      rec0_m_ready <= ((rec0_ready_dvec(rec0_ready_dvec'length - 1) and (not rec0_ready_dvec(rec0_ready_dvec'length - 2))) and (not spifi_T));
    end if;
      
    else
      rec0_rst_sync_vec <= (others => '0');
    end if;
    if (rec0_rst = '1') then
      rec0_data_valid <= '0';
    elsif (rec0_m_valid = '1') then
      rec0_data_valid <= '1';
    end if;
    rec0_rst <= (not rec0_rst_sync_vec(rec0_rst_sync_vec'length - 1) and rec0_rst_sync_vec(rec0_rst_sync_vec'length - 2));
  end if;
end process;


data_recorder_0_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec0_rst,
      clk                       => adc0_aclk_out,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc0_data,
      s_valid                   => adc0_valid,
      s_ready                   => open,

      m_data                    => rec0_m_data,
      m_valid                   => rec0_m_valid,
      m_ready                   => rec0_m_ready,
      
      compleat                  => open
    );


rec1_rst_proc : process(areset, adc1_aclk_out)
begin
  if (areset = '1') then
    rec1_rst <= '1';
    rec1_data_valid <= '0';
  elsif rising_edge(adc1_aclk_out) then
    if (spifi_switch_byte = x"01") then
      rec1_rst_sync_vec(rec1_rst_sync_vec'length - 1 downto 1) <= rec1_rst_sync_vec(rec1_rst_sync_vec'length - 2 downto 0);
      rec1_rst_sync_vec(0) <= spifi_cs;
      
    if (spifi_cs = '1') then
      rec1_ready_dvec <= (others => '0');
    else
      rec1_ready_dvec(rec1_ready_dvec'length - 1 downto 1) <= rec1_ready_dvec(rec1_ready_dvec'length - 2 downto 0);
      rec1_ready_dvec(0) <= spifi_s_ready;
      rec1_m_ready <= ((rec1_ready_dvec(rec1_ready_dvec'length - 1) and (not rec1_ready_dvec(rec1_ready_dvec'length - 2))) and (not spifi_T));
    end if;
      
    else
      rec1_rst_sync_vec <= (others => '0');
    end if;
    if (rec1_rst = '1') then
      rec1_data_valid <= '0';
    elsif (rec1_m_valid = '1') then
      rec1_data_valid <= '1';
    end if;
    rec1_rst <= (not rec1_rst_sync_vec(rec1_rst_sync_vec'length - 1) and rec1_rst_sync_vec(rec1_rst_sync_vec'length - 2));
  end if;
end process;


data_recorder_1_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec1_rst,
      clk                       => adc1_aclk_out,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc1_data,
      s_valid                   => adc1_valid,
      s_ready                   => open,

      m_data                    => rec1_m_data,
      m_valid                   => rec1_m_valid,
      m_ready                   => rec1_m_ready,
      
      compleat                  => open
    );

rec2_rst_proc : process(areset, adc2_aclk_out)
begin
  if (areset = '1') then
    rec2_rst <= '1';
    rec2_data_valid <= '0';
  elsif rising_edge(adc2_aclk_out) then
    if (spifi_switch_byte = x"02") then
      rec2_rst_sync_vec(rec2_rst_sync_vec'length - 1 downto 1) <= rec2_rst_sync_vec(rec2_rst_sync_vec'length - 2 downto 0);
      rec2_rst_sync_vec(0) <= spifi_cs;
      
    if (spifi_cs = '1') then
      rec2_ready_dvec <= (others => '0');
      rec2_m_ready <= '0';
    else
      rec2_ready_dvec(rec2_ready_dvec'length - 1 downto 1) <= rec2_ready_dvec(rec2_ready_dvec'length - 2 downto 0);
      rec2_ready_dvec(0) <= spifi_s_ready;
      rec2_m_ready <= ((rec2_ready_dvec(rec2_ready_dvec'length - 1) and (not rec2_ready_dvec(rec2_ready_dvec'length - 2))) and (not spifi_T));
    end if;
      
    else
      rec2_rst_sync_vec <= (others => '0');
    end if;
    if (rec2_rst = '1') then
      rec2_data_valid <= '0';
    elsif (rec2_m_valid = '1') then
      rec2_data_valid <= '1';
    end if;
    rec2_rst <= (not rec2_rst_sync_vec(rec2_rst_sync_vec'length - 1) and rec2_rst_sync_vec(rec2_rst_sync_vec'length - 2));
  end if;
end process;


data_recorder_2_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec2_rst,
      clk                       => adc2_aclk_out,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc2_data,
      s_valid                   => adc2_valid,
      s_ready                   => open,

      m_data                    => rec2_m_data,
      m_valid                   => rec2_m_valid,
      m_ready                   => rec2_m_ready,
      
      compleat                  => open
    );

rec3_rst_proc : process(areset, adc3_aclk_out)
begin
  if (areset = '1') then
    rec3_rst <= '1';
    rec3_data_valid <= '0';
  elsif rising_edge(adc3_aclk_out) then
    if (spifi_switch_byte = x"03") then
      rec3_rst_sync_vec(rec3_rst_sync_vec'length - 1 downto 1) <= rec3_rst_sync_vec(rec3_rst_sync_vec'length - 2 downto 0);
      rec3_rst_sync_vec(0) <= spifi_cs;
      
      if (spifi_cs = '1') then
        rec3_ready_dvec <= (others => '0');
      else
        rec3_ready_dvec(rec3_ready_dvec'length - 1 downto 1) <= rec3_ready_dvec(rec3_ready_dvec'length - 2 downto 0);
        rec3_ready_dvec(0) <= spifi_s_ready;
        rec3_m_ready <= ((rec3_ready_dvec(rec3_ready_dvec'length - 1) and (not rec3_ready_dvec(rec3_ready_dvec'length - 2))) and (not spifi_T));
      end if;
      
    else
      rec3_rst_sync_vec <= (others => '0');
    end if;
    if (rec3_rst = '1') then
      rec3_data_valid <= '0';
    elsif (rec3_m_valid = '1') then
      rec3_data_valid <= '1';
    end if;
    rec3_rst <= (not rec3_rst_sync_vec(rec3_rst_sync_vec'length - 1) and rec3_rst_sync_vec(rec3_rst_sync_vec'length - 2));
  end if;
end process;


data_recorder_3_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec3_rst,
      clk                       => adc3_aclk_out,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc3_data,
      s_valid                   => adc3_valid,
      s_ready                   => open,

      m_data                    => rec3_m_data,
      m_valid                   => rec3_m_valid,
      m_ready                   => rec3_m_ready,
      
      compleat                  => open
    );

adcx_calib_done <= adc3_valid & adc2_valid & adc1_valid & adc0_valid;
adcx_data_valid <= rec3_data_valid & rec2_data_valid & rec1_data_valid & rec0_data_valid;


spifi_sio <= PCS_O when spifi_T = '0' else (others => 'Z');

PCS_I <= spifi_sio;

spifi_switch_byte_switch_proc :
  process(spifi_sck, spifi_cs)
  begin
    if (spifi_cs = '1') then 
      spifi_T <= '1';
      spifi_cmd_counter <= '0';
    elsif falling_edge(spifi_sck) then
      if (spifi_cmd_valid = '1') then

        if (spifi_cmd_counter = '0') then
          spifi_cmd_counter <= '1';
          spifi_switch_byte <= spifi_cmd_byte;
          spifi_T <= '0'; 
        end if;

      end if;
    end if;
  end process;

spifi_mux_data_process :
  process (spifi_switch_byte, rec0_m_data, rec1_m_data, rec2_m_data, rec3_m_data, rec0_m_valid, rec1_m_valid, rec2_m_valid, rec3_m_valid)
  begin
    --rec0_m_ready <= 0;
    --rec1_m_ready <= 0;
    --rec2_m_ready <= 0;
    --rec3_m_ready <= 0;
     case spifi_switch_byte is
        when x"00" => spifi_s_data <= rec0_m_data;
                      spifi_s_valid <= rec0_m_valid;
                      --rec0_m_ready <= spifi_s_ready;
        when x"01" => spifi_s_data <= rec1_m_data;
                      spifi_s_valid <= rec1_m_valid;
                      --rec1_m_ready <= spifi_s_ready;
        when x"02" => spifi_s_data <= rec2_m_data;
                      spifi_s_valid <= rec2_m_valid;
                      --rec2_m_ready <= spifi_s_ready;
        when x"03" => spifi_s_data <= rec3_m_data;
                      spifi_s_valid <= rec3_m_valid;
                      --rec3_m_ready <= spifi_s_ready;
        when others => spifi_s_data <= rec0_m_data;
                       spifi_s_valid <= rec0_m_valid;
                      --rec0_m_ready <= spifi_s_ready;
     end case;
  end process;

spifi_module_inst : entity spifi_module
    generic map(
      C_CPHA            => '0',
      C_CPOL            => '0',
      C_LSB_FIRST       => false,
      C_NUM_QBURST      => C_BURST_WIDTH_SPIFI
    )
    port map( 
      SCK               => spifi_sck,
      CS                => spifi_cs,

      PCS_I             => PCS_I,
      PCS_O             => PCS_O,

      s_data            => spifi_s_data,
      s_valid           => spifi_s_valid,
      s_ready           => spifi_s_ready,

      cmd_byte          => spifi_cmd_byte,
      cmd_valid         => spifi_cmd_valid
    );
end Behavioral;
