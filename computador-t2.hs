type Address = Int
type Content = Int
type Reg = Int
type MemItem = (Address, Content)
type Mem = [MemItem]

type Signal = Bool
type IC = Reg
type IReg = (Reg, Reg)
type EQZ = Signal
type ACC = Reg
type RDM = Reg
type REM = Reg
data ControlSignals = ControlSignals {
  read_from_mem::Signal,
  write_to_mem::Signal,
  write_back_to_acc::Signal,
  write_back_to_pc::Signal
}
data State = State { mem::Mem, pc::IC, ireg::IReg, acc::ACC, eqz::EQZ, rdm::RDM, rem::REM, control::ControlSignals }

MEM_POS = 0
IC_POS = 1
IREG_POS = 2
ACC_POS = 3
EQZ_POS = 4
RDM_POS = 5
REM_POS = 6

LOD = 2
STO = 4
JMP = 6
JMZ = 8
CPE = 10
ADD = 14
SUB = 16
NOP = 18
HCF = 20


-- BUSCA, DECODIFICA, EXECUTA, ACESSA MEMORIA, ESCRITA EM BANCO REGISTRADORES
-- FETCH, DECODE, EXECUTE, MEMORY ACCESS, WRITE BACK
-- Memory antes de execute????
cpu :: State -> Maybe State
cpu (State { ireg = HCF }) = Nothing
cpu mState = do
    state <- mState
    state <- fetch state 
    state <- decode state
    state <- state { rem = snd (ireg state), rdm = acc state } -- joga <end> da instrução para o registrador REM, e o conteudo do ACC para RDM
    state <- memory state -- geralmente a etapa de memoria é depois da execução, mas nesse caso a ISA é explicita em definir que os conteúdos de memória são carregados para a instrucao executar sobre seu conteudo
    state <- execute state
    state <- write_back state -- apenas ao ajustar o PC, pois o ACC é melhor manipulado na etapa execute por conta da ULA

    -- ao finalizar, reseta registradores que não devem persistir entre ciclos e sinais de controle
    return state { ireg = (0,0), eqz = (0 == acc state), rdm = 0, rem = 0, control = {
        read_from_mem = False,
        write_to_mem = False,
        write_back_to_acc = False,
        write_back_to_pc = False,
    }}

fetch :: State -> State
fetch mState = do
    state <- state { rem = pc state }                           -- salva PC no registrador REM
    state <- memory_read state                                  -- Le REM da memoria e salva em RDM
    state <- state { ireg = (rdm state, 0) }                    -- Salva RDM na posicao alta de IReg
    state <- state { rem = (pc state + 1) }                     -- incrementa pc para a prox linha da memoria, o parametro da instrucao
    state <- memory_read state                                  -- Le REM da memoria e salva em RDM
    state <- state { ireg = (fst (ireg state), rdm state) }     -- Salva RDM na posicao baixa de IReg
    state <- state { rem = (pc state + 1) }                     -- incrementa PC para o proximo ciclo
    return state

decode :: State -> State
decode state | ins == STO = state { control = (control state) { write_to_mem = True } }
             | ins == LOD || ins == CPE || ins == ADD || ins == SUB = state { control = (control state) { write_back_to_acc = True, read_from_mem = True } }
             | ins == JMP = { control = (control state) { write_back_to_pc = True } }
             | ins == JMZ = { control = (control state) { write_back_to_pc = eqz control == True } }
             | otherwise = state
             where ins = fst (ireg state)


memory :: State -> State
memory state | read_from_mem (control state) = memory_read state
             | write_to_mem (control state)  = memory_write state
             | otherwise                     = state

execute :: State -> State
execute state | ins == CPE = state { acc = (if rdm control == acc control then 0 else 1) }
              | ins == ADD = state { acc = rdm + acc }
              | ins == SUB = state { acc = rdm - acc }
              | otherwise  = state

write_back :: State -> State
write_back state | write_back_to_pc (control state)  = state { pc = snd (ireg state) }
                 | otherwise = state

{-- CONTROLADOR DE MEMORIA --}
memory_read :: State -> State
memory_read state = state { rdm = select_address (rem (mem state)) (mem state) }

memory_write :: State -> State
memory_write state = state { mem = persist_address (rem mem) (rdm mem) mem }

select_address :: Address -> Mem -> Content
select_address _ [] = 0
select_address address (item:memory) | fst item == address = snd item
                                     | otherwise           = select_address address memory

persist_address :: Address -> Content -> Mem -> Mem
persist_address _ _ [] = []
persist_address address content (item:memory) | fst item == address = (address, content) : memory
                                              | otherwise           = item : (persist_address address content memory)

