library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library unisim;
use unisim.vcomponents.all;

entity DCM_PLL is
port
 (-- Clock in ports
  CLK_IN            : in     std_logic;
  -- Clock out ports
  CLK_0_OUT         : out    std_logic;
  CLK_45_OUT        : out    std_logic;
  CLK_90_OUT        : out    std_logic;
  CLK_135_OUT       : out    std_logic;
  CLK_180_OUT       : out    std_logic;
  CLK_225_OUT       : out    std_logic;
  CLK_270_OUT       : out    std_logic;
  CLK_315_OUT       : out    std_logic;
  -- Dynamic phase shift ports
  PSCLK             : in     std_logic;
  PSEN              : in     std_logic;
  PSINCDEC          : in     std_logic;
  PSDONE            : out    std_logic;
  -- Status and control signals
  DCM_RESET         : in     std_logic;
  PLL_RESET         : in     std_logic;
  STATUS            : out    std_logic_vector(2 downto 0);
  DCM_LOCKED        : out    std_logic;
  PLL_LOCKED        : out    std_logic
 );
end DCM_PLL;

architecture xilinx of DCM_PLL is
  signal clkin1            : std_logic;
  -- Output clock buffering
  signal clk_out1_internal : std_logic_vector(3 downto 0);
  signal pll_clk_out1_internal : std_logic_vector(3 downto 0);
  signal clkfb             : std_logic;
  signal clk               : std_logic_vector(3 downto 0);
  signal pll_clk           : std_logic_vector(3 downto 0);
  signal clkfbout          : std_logic;
  signal locked_internal   : std_logic;
  signal status_internal   : std_logic_vector(7 downto 0);
  
  signal pll_clkfbout      : std_logic;
  signal pll_clkfbin       : std_logic;
  constant ph45            : real := real(-45);
  
begin


  -- Input buffering
  --------------------------------------
  clkin1 <= CLK_IN;


  -- Clocking primitive
  --------------------------------------
  
  -- Instantiation of the DCM_PLL primitive
  --    * Unused inputs are tied off
  --    * Unused outputs are labeled unused
  dcm_sp_inst: DCM_SP
  generic map
   (CLKDV_DIVIDE          => 2.000,
    CLKFX_DIVIDE          => 1,
    CLKFX_MULTIPLY        => 4,
    CLKIN_DIVIDE_BY_2     => FALSE,
    CLKIN_PERIOD          => 8.0,
    CLKOUT_PHASE_SHIFT    => "VARIABLE",
    CLK_FEEDBACK          => "1X",
    DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS",
    PHASE_SHIFT           => 0,
    STARTUP_WAIT          => FALSE)
  port map
   -- Input clock
   (CLKIN                 => clkin1,
    CLKFB                 => clkfb,
    -- Output clocks
    CLK0                  => clk(0),
    CLK90                 => clk(1),
    CLK180                => clk(2),
    CLK270                => clk(3),
    CLK2X                 => open,
    CLK2X180              => open,
    CLKFX                 => open,
    CLKFX180              => open,
    CLKDV                 => open,
   -- Ports for dynamic phase shift
    PSCLK                 => PSCLK,
    PSEN                  => PSEN,
    PSINCDEC              => PSINCDEC,
    PSDONE                => PSDONE,
   -- Other control and status signals
    LOCKED                => locked_internal,
    STATUS                => status_internal,
    RST                   => DCM_RESET,
   -- Unused pin, tie low
    DSSEN                 => '0');

  STATUS                <= status_internal(2 downto 0);
  DCM_LOCKED            <= locked_internal;



  -- Output buffering
  -------------------------------------
  clkfb <= clk_out1_internal(0);

gen : for i in 0 to 3 generate
  clkout1_buf : BUFG
  port map
   (O   => clk_out1_internal(i),
    I   => clk(i));

  clkout2_buf : BUFG
  port map
   (O   => pll_clk_out1_internal(i),
    I   => pll_clk(i));


end generate;

    CLK_0_OUT <= clk_out1_internal(0);
   CLK_45_OUT <= pll_clk_out1_internal(0);
   CLK_90_OUT <= clk_out1_internal(1);
  CLK_135_OUT <= pll_clk_out1_internal(1);
  CLK_180_OUT <= clk_out1_internal(2);
  CLK_225_OUT <= pll_clk_out1_internal(2);
  CLK_270_OUT <= clk_out1_internal(3);
  CLK_315_OUT <= pll_clk_out1_internal(3);

PLL_BASE_inst : PLL_BASE
   generic map (
      BANDWIDTH => "OPTIMIZED",             -- "HIGH", "LOW" or "OPTIMIZED" 
      CLKFBOUT_MULT => 4,                   -- Multiply value for all CLKOUT clock outputs (1-64)
      CLKFBOUT_PHASE => 0.0,                -- Phase offset in degrees of the clock feedback output
                                            -- (0.0-360.0).
      CLKIN_PERIOD => 8.0,                  -- Input clock period in ns to ps resolution (i.e. 33.333 is 30
                                            -- MHz).
      -- CLKOUT0_DIVIDE - CLKOUT5_DIVIDE: Divide amount for CLKOUT# clock output (1-128)
      CLKOUT0_DIVIDE => 4,
      CLKOUT1_DIVIDE => 4,
      CLKOUT2_DIVIDE => 4,
      CLKOUT3_DIVIDE => 4,
      CLKOUT4_DIVIDE => 4,
      CLKOUT5_DIVIDE => 4,
      -- CLKOUT0_DUTY_CYCLE - CLKOUT5_DUTY_CYCLE: Duty cycle for CLKOUT# clock output (0.01-0.99).
      CLKOUT0_DUTY_CYCLE => 0.5,
      CLKOUT1_DUTY_CYCLE => 0.5,
      CLKOUT2_DUTY_CYCLE => 0.5,
      CLKOUT3_DUTY_CYCLE => 0.5,
      CLKOUT4_DUTY_CYCLE => 0.5,
      CLKOUT5_DUTY_CYCLE => 0.5,
      -- CLKOUT0_PHASE - CLKOUT5_PHASE: Output phase relationship for CLKOUT# clock output (-360.0-360.0).
      CLKOUT0_PHASE => 45.0,
      CLKOUT1_PHASE => 135.0,
      CLKOUT2_PHASE => 225.0,
      CLKOUT3_PHASE => 315.0,
      CLKOUT4_PHASE => 0.0,
      CLKOUT5_PHASE => 0.0,
      CLK_FEEDBACK => "CLKFBOUT",           -- Clock source to drive CLKFBIN ("CLKFBOUT" or "CLKOUT0")
      COMPENSATION => "SYSTEM_SYNCHRONOUS", -- "SYSTEM_SYNCHRONOUS", "SOURCE_SYNCHRONOUS", "EXTERNAL" 
      DIVCLK_DIVIDE => 1,                   -- Division value for all output clocks (1-52)
      REF_JITTER => 0.1,                    -- Reference Clock Jitter in UI (0.000-0.999).
      RESET_ON_LOSS_OF_LOCK => FALSE        -- Must be set to FALSE
   )
   port map (
      CLKFBOUT => pll_clkfbout, -- 1-bit output: PLL_BASE feedback output
      -- CLKOUT0 - CLKOUT5: 1-bit (each) output: Clock outputs
      CLKOUT0 => pll_clk(0),
      CLKOUT1 => pll_clk(1),
      CLKOUT2 => pll_clk(2),
      CLKOUT3 => pll_clk(3),
      CLKOUT4 => open,
      CLKOUT5 => open,
      LOCKED => PLL_LOCKED,     -- 1-bit output: PLL_BASE lock status output
      CLKFBIN => pll_clkfbin,   -- 1-bit input: Feedback clock input
      CLKIN => clk(0),       -- 1-bit input: Clock input
      RST => PLL_RESET            -- 1-bit input: Reset input
   );

pll_clkfbin <= pll_clkfbout;

end xilinx;
