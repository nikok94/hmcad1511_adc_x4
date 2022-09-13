
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.math_real.ALL;


library work;
--use work.true_dpram_ss_clk;
use work.async_ram_dual_port;
--use work.blk_mem_gen_v7_3_0;

entity data_recorder is
  generic (
    c_max_num_data            : integer := 16;
    c_data_width              : integer := 64;
    c_cnt_width               : integer := 32
  );
  Port ( 
    rst                       : in std_logic;

    mark_delay                : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
    mark_cnt                  : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

    stop                      : in std_logic;
    stop_cnt                  : in std_logic_vector(c_cnt_width - 1 downto 0);

    s_clk                     : in std_logic;
    s_cnt                     : in std_logic_vector(c_cnt_width - 1 downto 0);
    s_data                    : in std_logic_vector(c_data_width - 1 downto 0);
    s_valid                   : in std_logic;
    s_ready                   : out std_logic;

    m_clk                     : in std_logic;
    m_en                      : in std_logic;
    m_data                    : out std_logic_vector(c_data_width - 1 downto 0);
    m_valid                   : out std_logic;
    m_ready                   : in std_logic

  );
end data_recorder;

architecture Behavioral of data_recorder is
  type state_a_type          is (MASK_CNT_STATE, MASK_REC_STATE, WAIT_TRIG_STATE, RECORD_STATE, READY_STATE);
  signal state_a                : state_a_type;
  constant state_cnt_max        : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '1');
  signal addr_a                 : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal addr_a_cnt             : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal we_a                   : std_logic:= '0';
  signal en_a                   : std_logic:= '0';
  signal valid                  : std_logic:= '0';
  


  constant null_data            : std_logic_vector(c_data_width - 1 downto 0):= (others => '0');
  type state_b_type          is (IDLE, WAIT_DB, WAIT_M_READY0, ADDR_B_INC0, WAIT_DB1, WAIT_M_READY1, ADDR_B_INC1, CONT);
  signal state_b                : state_b_type;
  signal addr_b                 : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '0');
  signal en_b                   : std_logic:= '0';
  signal mark_delay_u32         : std_logic_vector(c_cnt_width - 1 downto 0);
  signal di_a                   : std_logic_vector(c_data_width - 1 downto 0);
  
begin
m_valid <= valid;
mark_delay_u32(mark_delay'length -1 downto 0) <= mark_delay;

en_a <= '1'; 

process(s_clk, rst)
begin
  if (rst = '1') then
    state_a <= MASK_CNT_STATE;
    s_ready <= '0';
    addr_a_cnt <= (others => '0');
    addr_a  <= (others => '0');
    di_a    <= (others => '0');
    we_a    <= '0'; 
  elsif rising_edge(s_clk) then
    case (state_a) is
      when MASK_CNT_STATE => -- wait mask trigger
        if (s_valid = '1') then
          if (s_cnt >= mark_delay_u32) then
            state_a <= MASK_REC_STATE; 
          end if;
        end if;
      when MASK_REC_STATE => -- mask recording
        if (s_valid = '1') then
          if (addr_a_cnt >= mark_cnt) then
            state_a <= WAIT_TRIG_STATE;
          end if;
          addr_a_cnt <= addr_a_cnt + 1;
          addr_a <= addr_a_cnt;
          di_a <= s_data;
          we_a <= '1'; 
        else
          we_a <= '0';
        end if;
      when WAIT_TRIG_STATE => -- wait trigger
        if (s_valid = '1') then
          if (addr_a_cnt < state_cnt_max) then
            addr_a_cnt <= addr_a_cnt + 1;
          else
            addr_a_cnt <= mark_cnt;
          end if;
          addr_a <= addr_a_cnt;
          di_a <= s_data;
          we_a <= '1'; 
        else
          we_a <= '0';
        end if;
        if (stop = '1') then
          state_a <= RECORD_STATE;
          s_ready <= '0';
        else
          s_ready <= '1';
        end if;
      when RECORD_STATE =>  
        if (s_valid = '1') then
          if (addr_a_cnt < state_cnt_max) then
            addr_a_cnt <= addr_a_cnt + 1;
          else
            addr_a_cnt <= mark_cnt;
          end if;
          addr_a <= addr_a_cnt;
          di_a <= s_data;
          we_a <= '1'; 
          if (s_cnt >= stop_cnt) then
            state_a <= READY_STATE;
          end if;
        else
          we_a <= '0';
        end if;
      when READY_STATE => 
      when others => --ready
    end case;
  end if;
end process;

--dpram_inst : entity true_dpram_ss_clk
--  Generic map(
--    c_data_width  => c_data_width,
--    c_data_num    => c_max_num_data
--  )
--  Port map
--  (
--    data    => s_data,
--    addr_a    => addr_a,
--    wea     => we_a,
--    s_clk     => s_clk,
--    q       => m_data
--  );

process(m_clk, rst)
begin
  if (rst = '1') then
    state_b <= IDLE;
    valid <= '0';
  elsif rising_edge(m_clk) then
    case (state_b) is
      when IDLE => -- ready
        if (state_a = READY_STATE) then
          state_b <= WAIT_DB;
          en_b <= '1';
        else
          en_b <= '0';
        end if;
        addr_b <= (others => '0');
      when WAIT_DB =>
        state_b <= WAIT_M_READY0;
      when WAIT_M_READY0 =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          addr_b <= addr_b + 1;
          state_b <= ADDR_B_INC0;
        else
          valid <= '1';
        end if;
      when ADDR_B_INC0 =>
        if (addr_b < mark_cnt) then
          state_b <= WAIT_M_READY0;
        else
          --if (addr_a < state_cnt_max) then
          --  addr_b <= addr_a + 1;
          --else
          --  addr_b <= mark_cnt;
          --end if;
          addr_b <= addr_a;
          state_b <= WAIT_DB1;
        end if;
      when WAIT_DB1 =>
        state_b <= WAIT_M_READY1;
      when WAIT_M_READY1 =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          if (addr_b < state_cnt_max) then
            addr_b <= addr_b + 1;
          else
            addr_b <= mark_cnt;
          end if;
          state_b <= ADDR_B_INC1;
        else
          valid <= '1';
        end if;
      when ADDR_B_INC1 =>
        if (addr_b = addr_a) then
          state_b <= CONT;
        else
          state_b <= WAIT_M_READY1;
        end if;
      when others => --ready
        en_b <= '0';
    end case;
  end if;
end process;

dpram_inst : entity async_ram_dual_port
  generic map (
    WIDTHA      => c_data_width,
    SIZEA       => c_max_num_data,
    ADDRWIDTHA  => addr_a'length,
    WIDTHB      => c_data_width,
    SIZEB       => c_max_num_data,
    ADDRWIDTHB  => addr_b'length
    )
  port map(
    clk_a       => s_clk,
    clk_b       => m_clk,
    en_a        => en_a,
    en_b        => en_b,
    we_a        => we_a,
    we_b        => '0',
    addr_a      => addr_a,
    addr_b      => addr_b,
    di_a        => di_a,
    di_b        => null_data,
    do_a        => open,
    do_b        => m_data
    );

end Behavioral;
