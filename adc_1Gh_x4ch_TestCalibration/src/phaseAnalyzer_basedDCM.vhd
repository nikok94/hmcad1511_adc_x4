----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 02.12.2020 12:31:21
-- Design Name: 
-- Module Name: phaseAnalyzer_basedDCM - Behavioral
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
use work.aFifo;

entity phaseAnalyzer_basedDCM is
    Generic(
      c_data_length     : integer := 4;
      c_num_out_state   : integer := 16;
      c_counter_width   : integer := 9
    );
    Port (
      dcm_clk_in    : in std_logic;
      clk_in        : in std_logic;
      rst_in        : in std_logic;
      continue_out  : out std_logic;
      data_in       : in std_logic_vector(c_data_length - 1 downto 0);
      result_out    : out std_logic_vector(c_num_out_state*(c_counter_width + 2*c_data_length)-1 downto 0)
    );
end phaseAnalyzer_basedDCM;

architecture Behavioral of phaseAnalyzer_basedDCM is
    constant sync_stage                 : integer := 3;
    constant sleep_delay                : integer := 10;
    signal DCM_COUNTER                  : std_logic_vector(c_counter_width - 1 downto 0);
    signal data_in_svec                 : std_logic_vector(c_data_length*sync_stage-1 downto 0); 

    signal adcx_trig_clk_out            : std_logic_vector(15 downto 0);
    constant trig_point_width           : integer := c_counter_width + c_data_length*2;
    
    type fclkPhaseType   is array (c_num_out_state - 1 downto 0) of std_logic_vector(trig_point_width-1 downto 0);
    signal fclkInfo                     : fclkPhaseType;

    signal DCM_PSCLK_d                  : std_logic;
    signal DCM_PSCLK_up                 : std_logic;
    signal DCM_PSCLK_down               : std_logic;
    signal DCM_PSCLK                    : std_logic;
    signal DCM_PSEN                     : std_logic;
    signal DCM_PSINCDEC                 : std_logic;
    signal DCM_PSDONE                   : std_logic;
    signal DCM_PSDONE_dvec              : std_logic_vector(sync_stage-1 downto 0);
    signal DCM_PSDONE_up                : std_logic;
    signal DCM_PSDONE_down              : std_logic;
    signal DCM_RESET                    : std_logic;
    signal DCM_LOCKED                   : std_logic;
    signal DCM_STATUS                   : std_logic_vector(7 downto 0);
    --signal DCM_COUNTER                  : integer;

    signal dcm_state                    : integer;
    signal dcm_div                      : std_logic_vector(1 downto 0);
    signal DCM_Sleep_counter            : integer;
    
    signal DCM_CLKIN                    : std_logic;
    
    signal DCM_CLKFB                    : std_logic;
    signal DCM_CLK2X0                   : std_logic;
    signal DCM_CLK2X180                 : std_logic;
    signal DCM_CLK1X0                   : std_logic;
    signal DCM_xCLK                     : std_logic_vector(2 downto 0);
    signal fclk_trig_vec                : std_logic_vector(c_data_length*2-1 downto 0):= (others => '0');
    signal fclk_trig_vec_sync           : std_logic_vector(c_data_length*2-1 downto 0);
    signal fclk_trig_vec_sync_t         : std_logic_vector(c_data_length*2-1 downto 0);
    signal aFifo_Empty_out              : std_logic;
    signal aFifo_EReadEn_in             : std_logic;
    signal DCM_SYNC                     : std_logic;
    signal DCM_CLK1X0_DIV2              : std_logic;
    signal DCM_CLK1X0_DIV2_d0           : std_logic;
    signal DCM_CLK1X0_DIV2_d1           : std_logic;
    signal WriteEn_in                   : std_logic;
    signal dcm_clk2x0_sync_vec          : std_logic_vector(2 downto 0);
    signal dcm_clk2x180_sync_vec        : std_logic_vector(2 downto 0);
                                        
    signal dcm_clk2x0_sync_up           : std_logic;
    signal dcm_clk2x0_sync_down         : std_logic;
    signal dcm_clk2x180_sync_up         : std_logic;

    signal fifo_data_in                 : std_logic_vector(c_data_length*2-1 downto 0);
    signal fclk_trig_vec_s              : std_logic_vector(c_data_length*2-1 downto 0);
    signal fclk_trig_vec_stable         : std_logic_vector(c_data_length*2-1 downto 0);
    signal fclk_trig_vec_stable_old     : std_logic_vector(c_data_length*2-1 downto 0);
    signal fclk_trig_vec_sd             : std_logic_vector(c_data_length*2-1 downto 0);

    signal stable_status                : std_logic_vector(c_data_length - 1 downto 0);
    signal stable_status_old            : std_logic_vector(c_data_length - 1 downto 0);
    signal fclk_cnt_ris                 : std_logic_vector(c_data_length - 1 downto 0);
    signal fclkInfo_cnt                 : integer range 0 to c_num_out_state - 1;
    signal continue                     : std_logic;

begin

--process(hmcad_x4_block_rst, clk_in)
--begin
--  if (hmcad_x4_block_rst = '1') then
--    dcm_state       <= 0;
--    DCM_PSCLK       <= '0';
--    DCM_PSEN        <= '0';
--    DCM_PSINCDEC    <= '0';
--    DCM_RESET       <= '1';
--    DCM_COUNTER     <= 0;
--    PLL_RESET <= '1';
--    dcm_div <= (others => '0');
--    DCM_Sleep_counter <= 0;
--  elsif rising_edge(clk_in) then
--    dcm_div <= dcm_div + 1; 
--    if (dcm_div = "00") then
--      DCM_PSCLK <= not DCM_PSCLK;
--    end if;
--    case (dcm_state) is
--      when 0 =>
--        DCM_RESET <= '1';
--        if (DCM_Sleep_counter < 125000) then
--          DCM_Sleep_counter <= DCM_Sleep_counter + 1;
--        else
--          dcm_state <= 7;
--        end if;
--      when 7 =>
--        DCM_RESET <= '0';
--        PLL_RESET <= '0';
--        if (DCM_LOCKED = '1') then
--          dcm_state <= 1;
--        end if;
--      when 1 => 
--        if ((m_fcb_wrreq = '1') and m_fcb_addr = SPIRegistersStrucrure'pos(TestReg)) then
--          DCM_PSINCDEC <= m_fcb_wrdata(0);
--          if (DCM_STATUS(0) = '1') then
--            if ((DCM_COUNTER > 0) and (m_fcb_wrdata(0) = '0')) then
--              dcm_state <= 2;
--            elsif ((DCM_COUNTER < 0) and (m_fcb_wrdata(0) = '1')) then
--              dcm_state <= 2;
--            end if;
--          else
--            dcm_state <= 2;
--          end if;
--        end if;
--      when 2 =>
--        if (DCM_PSCLK = '1') then
--          dcm_state <= 3;
--        end if;
--      when 3 =>
--        if (DCM_PSCLK = '0')then
--          DCM_PSEN <= '1';
--          dcm_state <= 4;
--        end if;
--      when 4 =>
--        if (DCM_PSCLK = '1') then
--          if (DCM_PSINCDEC = '1') then
--            DCM_COUNTER <= DCM_COUNTER + 1;
--          else
--            DCM_COUNTER <= DCM_COUNTER - 1;
--          end if;
--          dcm_state <= 5;
--        end if;
--      when 5 =>
--        if (DCM_PSCLK = '0')then
--          DCM_PSEN <= '0';
--          dcm_state <= 6;
--        end if;
--      when 6 =>
--        if (DCM_PSDONE = '1') then
--          dcm_state <= 1;
--        end if;
--      when others =>
--        dcm_state <= 0;
--    end case;
--  end if;
--end process;



continue_out <= continue;

process(rst_in, clk_in)
begin
  if (rst_in = '1') then
    dcm_state       <= 0;
    DCM_PSCLK       <= '0';
    DCM_PSEN        <= '0';
    DCM_PSINCDEC    <= '0';
    DCM_RESET       <= '1';
    dcm_div <= (others => '0');
    DCM_Sleep_counter <= 0;
    continue <= '0';
  elsif rising_edge(clk_in) then
    dcm_div <= dcm_div + 1; 
    
    fclk_trig_vec_sd <= fclk_trig_vec_s;    

    if (DCM_LOCKED = '0') then
      dcm_state <= 0;
      if (DCM_Sleep_counter < sleep_delay) then
        DCM_Sleep_counter <= DCM_Sleep_counter + 1;
      else
        DCM_RESET <= '0';
      end if;
    else 
      if (dcm_div = "00") then
        DCM_PSCLK <= not DCM_PSCLK;
      end if;    

      case (dcm_state) is
        when 0 =>
            DCM_Sleep_counter <= 0;
 
            if (DCM_STATUS(0) = '0') then
              if (DCM_PSCLK_down = '1') then
                DCM_PSINCDEC <= '1';
                DCM_PSEN <= '1';
                dcm_state <= 1;
              end if;
            else
              dcm_state <= 7;
              stable_status <= (others => '1');
              fclk_cnt_ris <= (others => '0');
              fclkInfo_cnt <= 0;
              fclkInfo <= (others => (others => '1'));
            end if; 
        
        when 1 =>
          if (DCM_PSEN = '1') then
            if (DCM_PSCLK_down = '1') then
              dcm_state <= 2;
              DCM_PSEN <= '0';  
            end if;
          end if;
              
        when 2 =>
          
          if (DCM_PSDONE_down = '1') then
            dcm_state <= 0;
          end if;
          
        when 3 =>
          if (DCM_Sleep_counter < sleep_delay) then
          
            DCM_Sleep_counter <= DCM_Sleep_counter + 1;
            i_loop : for i in 0 to data_in'length - 1 loop
              if (fclk_trig_vec_sd(2*i + 1 downto 2*i) /= fclk_trig_vec_s(2*i + 1 downto 2*i)) then
                stable_status(i) <= '0';
              end if;
            end loop;
            fclk_trig_vec_stable <= fclk_trig_vec_s;
          
          else
            
            for i in 0 to data_in'length - 1 loop
              if ((stable_status(i) = '1') and (stable_status_old(i) = '1')) then
                 stable_status_old <= stable_status;
                  
                 --if (fclk_trig_vec_stable(2*i+1) /= fclk_trig_vec_stable(2*i)) then
                   if (fclk_trig_vec_stable(2*i + 1 downto 2*i) /= fclk_trig_vec_stable_old(2*i + 1 downto 2*i)) then
                     fclk_cnt_ris(i) <= '1';
                     fclk_trig_vec_stable_old(2*i + 1 downto 2*i) <= fclk_trig_vec_stable(2*i + 1 downto 2*i);
                     fclkInfo(fclkInfo_cnt)(DCM_COUNTER'length + 2*c_data_length - 1 downto DCM_COUNTER'length) <= fclk_trig_vec_stable_old; 
                     fclkInfo(fclkInfo_cnt)(DCM_COUNTER'length - 1 downto 0) <= DCM_COUNTER;    
                   end if;
                 --end if;
              end if;
            end loop;
              
            dcm_state <= 8;
          end if;
          
        when 8 =>
            if (DCM_PSEN = '0') then
              if (DCM_PSCLK_down = '1') then
                DCM_PSINCDEC <= '0';
                DCM_PSEN <= '1';
              end if;            
            else
              if ((DCM_PSCLK_up = '1') and (DCM_PSEN = '1')) then
                dcm_state <= 4;
              end if;
            end if;
        when 4 =>
          if (fclk_cnt_ris /= 0) then
            if (fclkInfo_cnt <= c_num_out_state - 1) then
              fclkInfo_cnt <= fclkInfo_cnt + 1;          
              fclk_cnt_ris <= (others => '0');
            end if;
          end if;
          
          if (DCM_PSCLK_down = '1') then
            DCM_PSEN <= '0';
          end if;
          
          if (DCM_PSDONE_down = '1') then
            if (DCM_STATUS(0) = '1') then
              dcm_state <= 6;
            else
              dcm_state <= 5;
              DCM_Sleep_counter <= 0;
            end if;
          end if;
        when 5 =>
          if (DCM_Sleep_counter < sleep_delay) then
            if (fclkInfo_cnt = c_num_out_state - 1) then
              dcm_state <= 6;
            else
              DCM_Sleep_counter <= DCM_Sleep_counter + 1;
            end if;
          else
            DCM_Sleep_counter <= 0;
            dcm_state <= 3;
            stable_status <= (others => '1');
          end if;
        when 6 =>
          if (continue = '0') then
            continue <= '1';
            for i in 0 to c_data_length - 1 loop
              result_out(i*trig_point_width + trig_point_width - 1 downto i*trig_point_width) <= fclkInfo(i);
            end loop;
          end if;
        
        when 7 =>
          if (DCM_Sleep_counter < sleep_delay) then
            DCM_Sleep_counter <= DCM_Sleep_counter + 1;
            for i in 0 to data_in'length - 1 loop
              if (fclk_trig_vec_sd(2*i + 1 downto 2*i) /= fclk_trig_vec_s(2*i + 1 downto 2*i)) then
                stable_status(i) <= '0';
              end if;
            end loop;
          else
            DCM_Sleep_counter <= 0;
            stable_status_old <= stable_status;
            
            for i in 0 to c_data_length - 1 loop
              if ((stable_status(i) = '1') and (fclk_trig_vec_s(2*i + 1) /= fclk_trig_vec_s(2*i))) then
                fclk_trig_vec_stable_old(i*2 + 1 downto i*2) <= fclk_trig_vec_s(i*2 + 1 downto i*2);
              end if;
            end loop;
            
            stable_status <= (others => '1');
            dcm_state <= 3; 
          end if;
        when others =>
          dcm_state <= 0;
      end case;
    end if;
  end if;
end process;

DCM_COUNTER_proc :
  process(clk_in, DCM_RESET)
  begin
    if (DCM_RESET = '1') then
      DCM_COUNTER <= (others => '0');
      DCM_PSCLK_d <= '0';
      DCM_PSDONE_dvec <= (others => '0');
    elsif rising_edge(clk_in) then
      DCM_PSCLK_d <= DCM_PSCLK;
      DCM_PSCLK_up <= (not DCM_PSCLK_d) and DCM_PSCLK;
      DCM_PSCLK_down <= DCM_PSCLK_d and (not DCM_PSCLK);
      
      DCM_PSDONE_dvec(0) <= DCM_PSDONE;
      DCM_PSDONE_dvec(DCM_PSDONE_dvec'length - 1 downto 1) <= DCM_PSDONE_dvec(DCM_PSDONE_dvec'length - 2 downto 0);
      DCM_PSDONE_up <= (not DCM_PSDONE_dvec(DCM_PSDONE_dvec'length - 1)) and DCM_PSDONE_dvec(DCM_PSDONE_dvec'length - 2);
      DCM_PSDONE_down <= (DCM_PSDONE_dvec(DCM_PSDONE_dvec'length - 1)) and (not DCM_PSDONE_dvec(DCM_PSDONE_dvec'length - 2));
      
      if ((DCM_PSCLK_up = '1') and (DCM_STATUS(0) = '0')) then
        if (DCM_PSEN = '1') then
          if (DCM_PSINCDEC = '1') then
            DCM_COUNTER <= DCM_COUNTER + 1;
          else
            DCM_COUNTER <= DCM_COUNTER - 1;
          end if;
        end if;
      end if;
    end if;
  end process;


dcm_sp_inst: DCM_SP
  generic map
   (CLKDV_DIVIDE          => 2.000,
    CLKFX_DIVIDE          => 1,
    CLKFX_MULTIPLY        => 4,
    CLKIN_DIVIDE_BY_2     => FALSE,
    CLKIN_PERIOD          => 8.0,
    CLKOUT_PHASE_SHIFT    => "VARIABLE",
    CLK_FEEDBACK          => "2X",
    DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS",
    PHASE_SHIFT           => 0,
    STARTUP_WAIT          => FALSE)
  port map
   -- Input clock
   (CLKIN                 => DCM_CLKIN,
    CLKFB                 => DCM_CLKFB,
    -- Output clocks
    CLK0                  => DCM_xCLK(0),
    CLK90                 => open,
    CLK180                => open,
    CLK270                => open,
    CLK2X                 => DCM_xCLK(1),
    CLK2X180              => DCM_xCLK(2),
    CLKFX                 => open,
    CLKFX180              => open,
    CLKDV                 => open,
   -- Ports for dynamic phase shift
    PSCLK                 => DCM_PSCLK,
    PSEN                  => DCM_PSEN,
    PSINCDEC              => DCM_PSINCDEC,
    PSDONE                => DCM_PSDONE,
   -- Other control and status signals
    LOCKED                => DCM_LOCKED,
    STATUS                => DCM_STATUS,
    RST                   => DCM_RESET,
   -- Unused pin, tie low
    DSSEN                 => '0');

DCM_CLKIN <= dcm_clk_in;

  -- Output buffering
  -------------------------------------
  clkf_buf : BUFG 
  port map 
   (O => DCM_CLKFB,
    I => DCM_xCLK(1));

  clkout1_buf : BUFG
  port map
   (O   => DCM_CLK2X0,
    I   => DCM_xCLK(1));

  clkout2_buf : BUFG
  port map
   (O   => DCM_CLK2X180,
    I   => DCM_xCLK(2));

  clkout3_buf : BUFG
  port map
   (O   => DCM_CLK1X0,
    I   => DCM_xCLK(0));

  process(DCM_CLK2X0, DCM_LOCKED)
  begin
    if (DCM_LOCKED = '0') then
      WriteEn_in <= '0';
      dcm_clk2x0_sync_up <= '0';
    elsif rising_edge(DCM_CLK2X0) then
      dcm_clk2x0_sync_up <= not dcm_clk2x0_sync_up;
      if (dcm_clk2x0_sync_up = '1') then
        fifo_data_in <= fclk_trig_vec;
        WriteEn_in <= '1';
      else
        WriteEn_in <= '0';
      end if;
    end if;
  end process;

  fclki_gen : for i in 0 to 3 generate
    process(DCM_CLK2X0)
    begin
      if rising_edge(DCM_CLK2X0) then
        
        for i in 0 to sync_stage-2 loop
           data_in_svec((i+1)*data_in'length + data_in'length - 1 downto (i+1)*data_in'length) <= data_in_svec((i)*data_in'length + data_in'length - 1 downto (i)*data_in'length);
        end loop;

        data_in_svec(data_in'length - 1 downto 0) <= data_in;
      
      
        --fclk_trig_vec(2*i + 0) <= data_in(i);
        fclk_trig_vec(2*i + 0) <= data_in_svec((sync_stage-1)*data_in'length + i);
        fclk_trig_vec(2*i + 1) <= fclk_trig_vec(2*i + 0);
      end if;
    end process;
  end generate;

aFifo_inst : entity aFifo
    generic map(
        DATA_WIDTH => c_data_length*2,
        ADDR_WIDTH => 4
    )
    port map(
        -- Reading port.
        Data_out    => fclk_trig_vec_s,
        Empty_out   => aFifo_Empty_out,
        ReadEn_in   => aFifo_EReadEn_in,
        RClk        => clk_in,
        -- Writing port.
        Data_in     => fifo_data_in,
        Full_out    => open,
        WriteEn_in  => WriteEn_in,
        WClk        => DCM_CLK2X0,

        Clear_in    => DCM_RESET
    );
--  fclk_trig_vec_s <= fifo_data_in;
  
  
  
  aFifo_EReadEn_in <= not aFifo_Empty_out;


end Behavioral;
