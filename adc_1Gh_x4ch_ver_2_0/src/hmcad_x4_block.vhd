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

library work;
use work.hmcad_adc_block;
use work.async_fifo_x64;

entity hmcad_x4_block is
  generic (
    c_channel_number        : integer := 4
  );
  Port (
    clk                     : in std_logic;
    areset                  : in std_logic;
    enable                  : in std_logic_vector(c_channel_number - 1 downto 0);
    calib_done              : out std_logic_vector(c_channel_number - 1 downto 0);

    adcx_lclk_p             : in std_logic_vector(c_channel_number - 1 downto 0);
    adcx_lclk_n             : in std_logic_vector(c_channel_number - 1 downto 0);
    adcx_fclk_p             : in std_logic_vector(c_channel_number - 1 downto 0);
    adcx_fclk_n             : in std_logic_vector(c_channel_number - 1 downto 0);
    adcx_dx_a_p             : in std_logic_vector(c_channel_number * 4 - 1 downto 0);
    adcx_dx_a_n             : in std_logic_vector(c_channel_number * 4 - 1 downto 0);
    adcx_dx_b_p             : in std_logic_vector(c_channel_number * 4 - 1 downto 0);
    adcx_dx_b_n             : in std_logic_vector(c_channel_number * 4 - 1 downto 0);

    adcx_clk_out            : out std_logic_vector(c_channel_number -1 downto 0);
    
    cnt_rst                 : in std_logic;
    cnt_out                 : out std_logic_vector(c_channel_number*32 - 1 downto 0);
    
    data_out                : out std_logic_vector(c_channel_number*64 - 1 downto 0);
    valid_out               : out std_logic_vector(c_channel_number -1 downto 0)

    );
end hmcad_x4_block;

architecture Behavioral of hmcad_x4_block is
  signal gclk                           : std_logic_vector(c_channel_number - 1 downto 0);
  signal data                           : std_logic_vector(c_channel_number*96 - 1 downto 0);
  signal valid                          : std_logic_vector(c_channel_number - 1 downto 0);
  signal fifo_data                      : std_logic_vector(c_channel_number*96 - 1 downto 0);
  signal fifo_empty                     : std_logic_vector(c_channel_number - 1 downto 0);
  signal fifo_valid                     : std_logic_vector(c_channel_number - 1 downto 0);
  signal fifo_rd_en                     : std_logic_vector(c_channel_number - 1 downto 0);

begin

adcx_clk_out <= gclk;

adc_block_gen : for i in 0 to c_channel_number - 1 generate
  adc_inst : entity hmcad_adc_block 
    Port map(
      lclk_p                    => adcx_lclk_p(i),
      lclk_n                    => adcx_lclk_n(i),
      fclk_p                    => adcx_fclk_p(i),
      fclk_n                    => adcx_fclk_n(i),
      dx_a_p                    => adcx_dx_a_p(i*4 + 3 downto i*4),
      dx_a_n                    => adcx_dx_a_n(i*4 + 3 downto i*4),
      dx_b_p                    => adcx_dx_b_p(i*4 + 3 downto i*4),
      dx_b_n                    => adcx_dx_b_n(i*4 + 3 downto i*4),

      areset                    => areset,
      enable                    => enable(i),
      calib_done                => calib_done(i),
      gclk_out                  => gclk(i),

      cnt_rst                   => cnt_rst,
      cnt_out                   => data(i * 96 + 95 downto i * 96 + 64),

      data_out                  => data(i * 96 + 63 downto i * 96 + 0),
      frame_out                 => open,
      valid_out                 => valid(i)
    );
  
  fifo_rd_en(i) <= not fifo_empty(i);
  valid_out(i) <= fifo_valid(i);
  data_out(i * 64 + 63 downto i * 64 + 0) <= fifo_data(i * 96 + 63 downto i * 96 + 0); 
  cnt_out(i * 32 + 31 downto i * 32 + 0) <= fifo_data(i * 96 + 95 downto i * 96 + 64);

  async_fifo_x64_inst: entity async_fifo_x64
    port map(
        rst                     => areset, 
        wr_clk                  => gclk(i),
        rd_clk                  => clk, 
        din                     => data(i * 96 + 95 downto i * 96),
        wr_en                   => valid(i),
        rd_en                   => fifo_rd_en(i),
        dout                    => fifo_data(i * 96 + 95 downto i * 96),
        full                    => open,
        empty                   => fifo_empty(i),
        valid                   => fifo_valid(i)
    );
end generate;
end Behavioral;
