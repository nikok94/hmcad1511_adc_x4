----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.12.2019 10:05:15
-- Design Name: 
-- Module Name: hmcad_x4_top - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.HMCAD1511_v3_00;
use work.clock_generator;
use work.spi_master;
use work.hmcad_x4_block;

entity hmcad_x4_top is
    Port (
        in_clk_50MHz            : in std_logic;
        in_clk_20MHz            : in std_logic;
        xc_sys_rstn             : in std_logic;
    
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

        fpga_sck                : in std_logic;
        fpga_cs                 : in std_logic;
        fpga_miso               : out std_logic;
        fpga_mosi               : in std_logic;
        
        spifi_cs                : in std_logic;
        spifi_sck               : in std_logic;
        spifi_sio               : inout std_logic_vector(3 downto 0);
        
        int_adcx                : out std_logic_vector(3 downto 0);
        
        i2c_sda                 : in std_logic;
        i2c_scl                 : in std_logic;
        
        dd                      : inout std_logic_vector(7 downto 0);
        clk_dd                  : in std_logic;
        cs_dd                   : in std_logic;
        
        pulse_n                 : out std_logic := '0';
        pulse_p                 : out std_logic := '0'

        );
end hmcad_x4_top;

architecture Behavioral of hmcad_x4_top is
    constant C_BURST_WIDTH_SPIFI        : integer := 16;
    signal lclk_0                       : std_logic;
    signal lclk_1                       : std_logic;
    signal lclk_2                       : std_logic;
    signal lclk_3                       : std_logic;
    
    signal adc0_aclk_out                : std_logic;
    signal adc0_valid                   : std_logic;
    signal adc0_data                    : std_logic_vector(63 downto 0);
    
    signal adc1_aclk_out                : std_logic;
    signal adc1_valid                   : std_logic;
    signal adc1_data                    : std_logic_vector(63 downto 0);
    
    signal adc2_aclk_out                : std_logic;
    signal adc2_valid                   : std_logic;
    signal adc2_data                    : std_logic_vector(63 downto 0);

    signal adc3_aclk_out                : std_logic;
    signal adc3_valid                   : std_logic;
    signal adc3_data                    : std_logic_vector(63 downto 0);
    
    signal sys_rst                      : std_logic;
    signal pll_lock                     : std_logic;
    signal clk_125MHz                   : std_logic;
    signal clk_250MHz                   : std_logic;
    signal rst                          : std_logic;
    signal infrst_rst_out               : std_logic;
    
    signal MISO_I                       : std_logic;
    signal MISO_O                       : std_logic;
    signal MISO_T                       : std_logic;
    signal MOSI_I                       : std_logic;
    signal MOSI_O                       : std_logic;
    signal MOSI_T                       : std_logic;
    signal m_fcb_aresetn                : std_logic;
    signal m_fcb_addr                   : std_logic_vector(8 - 1 downto 0);
    signal m_fcb_wrdata                 : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_wrreq                  : std_logic;
    signal m_fcb_wrack                  : std_logic;
    signal m_fcb_rddata                 : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_rdreq                  : std_logic;
    signal m_fcb_rdack                  : std_logic;
    
    signal reg_address_int              : integer;
    
    signal adcx_calib_done              : std_logic_vector(3 downto 0);
    signal adcx_data_valid              : std_logic_vector(3 downto 0);
    signal acfg_bits                    : std_logic_vector(15 downto 0);
    signal aext_trig                    : std_logic;
    signal trig_start                   : std_logic;
    signal trig_position                : std_logic_vector(15 downto 0);

begin

rst <= infrst_rst_out;

dd(0) <= pll_lock;
dd(1) <= adcx_calib_done(0) and adcx_calib_done(1) and adcx_calib_done(2);
dd(dd'length - 1 downto 2) <= (others => 'Z');

sys_rst <= (not xc_sys_rstn);

Clock_gen_inst : entity clock_generator
    Port map( 
      clk_in            => in_clk_20MHz,
      rst_in            => sys_rst,
      pll_lock          => pll_lock,
      clk_out_125MHz    => clk_125MHz,
      clk_out_250MHz    => clk_250MHz,
      rst_out           => infrst_rst_out
    );


spi_fcb_master_inst : entity spi_master
    generic map(
      C_CPHA            => '1',
      C_CPOL            => '1',
      C_LSB_FIRST       => false
    )
    Port map( 
      SCK               => fpga_sck,
      CS                => fpga_cs,

      MISO_I            => MISO_I,
      MISO_O            => MISO_O,
      MISO_T            => MISO_T,
      MOSI_I            => MOSI_I,
      MOSI_O            => MOSI_O,
      MOSI_T            => MOSI_T,

      m_fcb_clk         => clk_125MHz,
      m_fcb_areset      => infrst_rst_out,
      m_fcb_addr        => m_fcb_addr   ,
      m_fcb_wrdata      => m_fcb_wrdata ,
      m_fcb_wrreq       => m_fcb_wrreq  ,
      m_fcb_wrack       => m_fcb_wrack  ,
      m_fcb_rddata      => m_fcb_rddata ,
      m_fcb_rdreq       => m_fcb_rdreq  ,
      m_fcb_rdack       => m_fcb_rdack  
    );

OBUFT_inst : OBUFT
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => fpga_miso,     -- Buffer output (connect directly to top-level port)
      I => MISO_O,     -- Buffer input
      T => MISO_T      -- 3-state enable input 
   );

MOSI_I <= fpga_mosi;

-------------------------------------------------
-- управляющие регистры 
-------------------------------------------------
-- процесс записи/чтения регистров управления
reg_address_int <= conv_integer(m_fcb_addr(6 downto 0));

--m_fcb_wr_process :
--    process(clk_125MHz)
--    begin
--      if rising_edge(clk_125MHz) then
--        if (infrst_rst_out = '1') then
--          wr_req_vec <= (others => '0');
--          control_reg(15 downto 0) <= (others => '0');
--          trig_set_up_reg(15 downto 8) <= x"7f";
--          trig_set_up_reg(3 downto 0) <= (others => '0');
--          low_adc_buff_len <= x"2004";
--          trig_window_width_reg <= x"0200";
--          calib_pattern_reg <= x"55AA";
--          adc_calib <= '0';
--        elsif (m_fcb_wrreq = '1') then
--          m_fcb_wrack <= '1';
--          case reg_address_int is
--            when 0 => 
--              wr_req_vec(0) <= '1';
--              trig_set_up_reg(15 downto 2) <= m_fcb_wrdata(15 downto 2);
--            when 1 => 
--              wr_req_vec(1) <= '1';
--              trig_window_width_reg <= m_fcb_wrdata;
--            when 2 =>
--              wr_req_vec(2) <= '1';
--              trig_position_reg <= m_fcb_wrdata;
--            when 3 =>
--              wr_req_vec(3) <= '1';
--              trig_set_up_reg(1 downto 0) <= m_fcb_wrdata(3 downto 2);
--              control_reg(1 downto 0) <= m_fcb_wrdata(1 downto 0);
--              control_reg(7 downto 4) <= m_fcb_wrdata(7 downto 4);
--            when 4 =>
--              wr_req_vec(4) <= '1';
--              calib_pattern_reg <= m_fcb_wrdata;
--            when 5 =>
--              wr_req_vec(5) <= '1';
--              low_adc_buff_len <= m_fcb_wrdata;
--            when 6 => 
--              pulse_start <= m_fcb_wrdata(0);
--            when others =>
--          end case;
--        else 
--          m_fcb_wrack               <= '0';
--          wr_req_vec                <= (others => '0');
--          control_reg(15 downto 0)  <= (others => '0');
--          pulse_start               <= '0';
--          adc_calib                 <= control_reg(0);
--        end if;
--      end if;
--    end process;

m_fcb_rd_process :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        if (m_fcb_rdreq = '1') then
          m_fcb_rdack <= '1';
          case reg_address_int is
            when 0 => 
              m_fcb_rddata <= adc0_data(15 downto 0);
            when 1 => 
              m_fcb_rddata <= adc0_data(31 downto 16);
            when 2 =>
              m_fcb_rddata <= adc0_data(47 downto 32);
            when 3 =>
              m_fcb_rddata <= adc0_data(63 downto 48);
            when 4 =>
              m_fcb_rddata <= adc1_data(15 downto 0);
            when 5 => 
              m_fcb_rddata <= adc1_data(31 downto 16);
            when 6 =>
              m_fcb_rddata <= adc1_data(47 downto 32);
            when 7 =>
              m_fcb_rddata <= adc1_data(63 downto 48);
            when 8 =>
              m_fcb_rddata <= adc2_data(15 downto 0);
            when 9 => 
              m_fcb_rddata <= adc2_data(31 downto 16);
            when 10 =>
              m_fcb_rddata <= adc2_data(47 downto 32);
            when 11 =>
              m_fcb_rddata <= adc2_data(63 downto 48);
            when 12 => 
              m_fcb_rddata <= adc3_data(15 downto 0);
            when 13 => 
              m_fcb_rddata <= adc3_data(31 downto 16);
            when 14 =>
              m_fcb_rddata <= adc3_data(47 downto 32);
            when 15 =>
              m_fcb_rddata <= adc3_data(63 downto 48);
            when 16 =>
              m_fcb_rddata(3 downto 0) <= adc3_valid & adc2_valid & adc1_valid & adc0_valid;
            when others =>
          end case;
        else 
          m_fcb_rdack <= '0';
        end if;
      end if;
    end process;

hmcad_x4_block_inst : entity hmcad_x4_block
  Port map(
    areset                 => rst,
    acfg_bits              => acfg_bits,
    aext_trig              => aext_trig,

    trig_start             => trig_start,
    trig_position          => trig_position,
    
    adc0_lclk_p             => adc0_lclk_p,
    adc0_lclk_n             => adc0_lclk_n,
    adc0_fclk_p             => adc0_fclk_p,
    adc0_fclk_n             => adc0_fclk_n,
    adc0_dx_a_p             => adc0_dx_a_p,
    adc0_dx_a_n             => adc0_dx_a_n,
    adc0_dx_b_p             => adc0_dx_b_p,
    adc0_dx_b_n             => adc0_dx_b_n,

    adc1_lclk_p             => adc1_lclk_p,
    adc1_lclk_n             => adc1_lclk_n,
    adc1_fclk_p             => adc1_fclk_p,
    adc1_fclk_n             => adc1_fclk_n,
    adc1_dx_a_p             => adc1_dx_a_p,
    adc1_dx_a_n             => adc1_dx_a_n,
    adc1_dx_b_p             => adc1_dx_b_p,
    adc1_dx_b_n             => adc1_dx_b_n,

    adc2_lclk_p             => adc2_lclk_p,
    adc2_lclk_n             => adc2_lclk_n,
    adc2_fclk_p             => adc2_fclk_p,
    adc2_fclk_n             => adc2_fclk_n,
    adc2_dx_a_p             => adc2_dx_a_p,
    adc2_dx_a_n             => adc2_dx_a_n,
    adc2_dx_b_p             => adc2_dx_b_p,
    adc2_dx_b_n             => adc2_dx_b_n,

    adc3_lclk_p             => adc3_lclk_p,
    adc3_lclk_n             => adc3_lclk_n,
    adc3_fclk_p             => adc3_fclk_p,
    adc3_fclk_n             => adc3_fclk_n,
    adc3_dx_a_p             => adc3_dx_a_p,
    adc3_dx_a_n             => adc3_dx_a_n,
    adc3_dx_b_p             => adc3_dx_b_p,
    adc3_dx_b_n             => adc3_dx_b_n,

    adcx_calib_done         => adcx_calib_done,
    adcx_data_valid         => adcx_data_valid,

    spifi_cs                => spifi_cs ,
    spifi_sck               => spifi_sck,
    spifi_sio               => spifi_sio
    );

  int_adcx <= adcx_data_valid and adcx_calib_done;

end Behavioral;
