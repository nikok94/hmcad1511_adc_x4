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
use work.trigger_capture;
use work.data_recorder;
use work.hmcad_adc_block;
use work.aFifo;

entity hmcad_x4_block is
  generic (
    c_max_num_data          : integer := 128;
    c_data_width            : integer := 64
  );
  Port (
    areset                  : in std_logic;
    TriggerSetUp            : in std_logic_vector(15 downto 0);
    ADCEnableReg            : in std_logic_vector(15 downto 0);
    TriggerPositionSetUp    : in std_logic_vector(15 downto 0);
    mode                    : in std_logic_vector(1 downto 0);
    start                   : in std_logic;
    adcx_lclk_p             : in std_logic_vector(3 downto 0);
    adcx_lclk_n             : in std_logic_vector(3 downto 0);
    adcx_fclk_p             : in std_logic_vector(3 downto 0);
    adcx_fclk_n             : in std_logic_vector(3 downto 0);
    adcx_dx_a_p             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_dx_a_n             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_dx_b_p             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_dx_b_n             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_calib_done         : out std_logic_vector(3 downto 0);
    adcx_interrupt          : out std_logic_vector(3 downto 0);
    adcx_tick_ms            : out std_logic_vector(3 downto 0);
    adcx_fclk_out           : out std_logic_vector(3 downto 0);
    
    slave_x_clk             : out std_logic_vector(4 - 1 downto 0);
    slave_x_valid           : out std_logic_vector(4 - 1 downto 0);
    slave_x_ready           : in std_logic_vector(4 - 1 downto 0);
    slave_x_data            : out std_logic_vector(4*c_data_width - 1 downto 0);
    slave_x_cs_up           : in std_logic_vector(4 - 1 downto 0);

    recorder_rst            : in std_logic


    );
end hmcad_x4_block;

architecture Behavioral of hmcad_x4_block is
  constant C_BURST_WIDTH_SPIFI          : integer := 16;
  constant bitslip_delay                : integer := 10;
  constant calid_done_delay             : integer := 10000000;
  
  constant num_data                     : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '1');

  signal reg_address_int                : integer;
  
  signal mux_data                       : std_logic_vector(63 downto 0);
  signal mux_data_selector              : std_logic_vector(1 downto 0);
  
  signal spifi_T                        : std_logic;
  signal spifi_cmd_counter              : std_logic;
  signal spifi_switch_byte              : std_logic_vector(7 downto 0);
  
  signal spifi_cmd_byte                 : std_logic_vector(7 downto 0);
  signal spifi_cmd_valid                : std_logic;
  
  signal PCS_I                          : std_logic_vector(3 downto 0);
  signal PCS_O                          : std_logic_vector(3 downto 0);
  signal spifi_s_data                   : std_logic_vector(C_BURST_WIDTH_SPIFI*4 - 1 downto 0);
  signal spifi_s_valid                  : std_logic;
  signal spifi_s_ready                  : std_logic;
  signal spifi_s_ready_sync             : std_logic;
  signal spifi_s_ready_dvec             : std_logic_vector(1 downto 0);
  signal spifi_cs_dvec                  : std_logic_vector(2 downto 0);
  signal spifi_cs_up                    : std_logic;
  
  signal trigger_start                  : std_logic;
  signal capture_mode                   : std_logic_vector(1 downto 0);
  signal front_condition                : std_logic_vector(1 downto 0);
  signal capture_level                  : std_logic_vector(7 downto 0);
  signal start_offset                   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

  signal state                          : integer;
  constant frame_sync_pattern           : std_logic_vector(7 downto 0) := x"0F";
  signal adcx_enable                    : std_logic_vector(3 downto 0) := "1111";
  signal TriggerSetUp_d                 : std_logic_vector(15 downto 0);
  signal ADCEnableReg_d                 : std_logic_vector(15 downto 0);
  signal TriggerPositionSetUp_d         : std_logic_vector(15 downto 0);
  signal mode_d                         : std_logic_vector(1 downto 0);
  signal trig_position                  : std_logic_vector(15 downto 0);
  signal tick_counter                   : integer;
  signal adcx_calib_status              : std_logic_vector(3 downto 0);
  signal start_dvec                     : std_logic_vector(3 downto 0);
  signal start_sync                     : std_logic;
  
  signal adcx_gclk                      : std_logic_vector(3 downto 0);
  signal adcx_gclk_out                  : std_logic_vector(3 downto 0);
  signal adcx_gclkdiv2                  : std_logic_vector(3 downto 0);
  type x4datatype   is array(3 downto 0) of std_logic_vector(63 downto 0);
  signal adcx_data                      : x4datatype;
  signal adcx_data_d0                   : x4datatype;
  signal adcx_data_d1                   : x4datatype;
  signal recx_ready                     : std_logic_vector(3 downto 0);
  signal recx_valid                     : std_logic_vector(3 downto 0);
  signal adcx_valid                     : std_logic_vector(3 downto 0);
  signal adcx_valid_d1                  : std_logic;
  signal recx_rst                       : std_logic_vector(3 downto 0);
  signal trigger_enable                 : std_logic_vector(3 downto 0);
  signal trigger_out                    : std_logic_vector(3 downto 0);
  signal trigger_data_in                : std_logic_vector(63 downto 0);
  
  signal fifocnt                        : std_logic_vector(1 downto 0);
  signal trigger                        : std_logic;
  signal recorder_rst_vec               : std_logic_vector(3 downto 0);
  
  signal adcx_calib_done_out            : std_logic_vector(3 downto 0);
  signal adcx_frame_ibufds              : std_logic_vector(3 downto 0);
  signal all_calib_done                 : std_logic;
  
  signal Empty_out                      : std_logic_vector(3 downto 0);
  signal ReadEn_in                      : std_logic;
  signal main_clk                       : std_logic;
  signal Clear_in                       : std_logic;
  signal all_valid                      : std_logic;
  
begin
trig_position <= TriggerPositionSetUp;
adcx_enable <= ADCEnableReg(3 downto 0);
mux_data_selector <= TriggerSetUp(3 downto 2);
adcx_fclk_out <= adcx_frame_ibufds;

main_clk <= adcx_gclk(0);

adc_block_gen : for i in 3 downto 0 generate

adc0_inst : entity hmcad_adc_block
  Port map(
    areset                => areset,
    lclk_p                => adcx_lclk_p(i),
    lclk_n                => adcx_lclk_n(i),
    fclk_p                => adcx_fclk_p(i),
    fclk_n                => adcx_fclk_n(i),
    dx_a_p                => adcx_dx_a_p(i*4 + 3 downto i*4),
    dx_a_n                => adcx_dx_a_n(i*4 + 3 downto i*4),
    dx_b_p                => adcx_dx_b_p(i*4 + 3 downto i*4),
    dx_b_n                => adcx_dx_b_n(i*4 + 3 downto i*4),

    fclk_ibufgds          => adcx_frame_ibufds(i),
    gclk_out              => adcx_gclk(i),
    gclkdiv2_out          => adcx_gclkdiv2(i),
    gclkdiv4_out          => adcx_tick_ms(i),

    data                  => adcx_data(i),
    data_valid            => adcx_valid(i)
  );

aFifo_inst : entity aFifo
    generic map(
        DATA_WIDTH => 64,
        ADDR_WIDTH => 4
    )
    port map(
        -- Reading port.
        Data_out    => adcx_data_d0(i),
        Empty_out   => Empty_out(i),
        ReadEn_in   => ReadEn_in,
        RClk        => main_clk,
        -- Writing port.
        Data_in     => adcx_data(i),
        Full_out    => open,
        WriteEn_in  => adcx_valid(i),
        WClk        => adcx_gclk(i),

        Clear_in    => Clear_in
    );

data_recorder_inst :  entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_start_delay             => 7
    )
    Port map( 
      rst                       => recx_rst(i),
      clk                       => main_clk,

      start                     => trigger,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adcx_data_d0(i),
      s_valid                   => ReadEn_in,
      s_ready                   => open,

      m_data                    => slave_x_data(i*64 + 63 downto i*64),
      m_valid                   => recx_valid(i),
      m_ready                   => slave_x_ready(i),
      
      compleat                  => open
    );

recorder_irq_proc :
  process(main_clk, recx_rst(i))
  begin
    if (recx_rst(i) = '1') then
      adcx_interrupt(i) <= '0';
    elsif rising_edge(main_clk) then
      if (recx_valid(i) = '1') then
        adcx_interrupt(i) <= '1';
      end if;
    end if;
  end process;

  recx_rst(i) <= ((recorder_rst or (not adcx_enable(i))) or slave_x_cs_up(i));
  adcx_calib_done_out(i) <= (adcx_valid(i) or (not adcx_enable(i)));
  slave_x_clk(i) <= main_clk;
  slave_x_valid(i) <= recx_valid(i);

end generate;


Clear_in <= (not all_calib_done);
all_valid <= not ((Empty_out(0) and adcx_enable(0)) or (Empty_out(1) and adcx_enable(1)) or (Empty_out(2) and adcx_enable(2)) or (Empty_out(3) and adcx_enable(3)));

process(main_clk, all_valid)
begin
  if (all_valid = '0') then
    fifocnt <= (others => '0');
    ReadEn_in <= '0';
  elsif rising_edge(main_clk) then
    if (fifocnt /= "11") then
      fifocnt <= fifocnt + 1;
    else
      ReadEn_in <= '1'; 
    end if;
  end if;
end process;


adcx_calib_done <= adcx_calib_done_out;

all_calib_done <= adcx_calib_done_out(3) and adcx_calib_done_out(2) and adcx_calib_done_out(1) and adcx_calib_done_out(0);

trigger_capture_inst : entity trigger_capture
    generic map(
        c_data_width    => 64
    )
    port map( 
      clk               => main_clk,
      rst               => recorder_rst,

      capture_mode      => mode,
      front_condition   => TriggerSetUp(5 downto 4),

      capture_level     => TriggerSetUp(15 downto 8),

      trigger_set_up    => start,
      data              => trigger_data_in,
      vector_valid      => open,
      ext_trig          => '0',
      
      l_up              => open,
      l_down            => open,
      
      trigger_start     => trigger
    );

process(mux_data_selector, adcx_data_d0)
begin
  trigger_data_in <= (others => '0');
  case(mux_data_selector) is
    when "00" =>
      trigger_data_in <= adcx_data_d0(0);
    when "01" => 
      trigger_data_in <= adcx_data_d0(1);
    when "10" =>
      trigger_data_in <= adcx_data_d0(2);
    when "11" =>
      trigger_data_in <= adcx_data_d0(3);
    when others =>
  end case;
end process;

end Behavioral;
