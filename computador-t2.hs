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

data ControlSignals = ControlSignals
  { read_from_mem :: Signal,
    write_to_mem :: Signal,
    write_back_to_acc :: Signal,
    write_back_to_pc :: Signal
  }

data State = State {mem :: Mem, pc :: IC, ireg :: IReg, acc :: ACC, eqz :: EQZ, rdm :: RDM, regMemAdd :: REM, control :: ControlSignals}

lod = 2

sto = 4

jmp = 6

jmz = 8

cpe = 10

add = 14

sub = 16

nop = 18

hcf = 20

-- FETCH, DECODE, EXECUTE, MEMORY ACCESS, WRITE BACK
-- Memory antes de execute????
cpu :: State -> State
cpu state = finalState
  where
    state1 = fetch state
    state2 = decode state1
    state3 = state2 {regMemAdd = snd (ireg state2), rdm = acc state2}
    state4 = memory state3
    state5 = execute state4
    state6 = writeBack state5
    isHalt = fst (ireg state) == hcf
    state7 = state6 {ireg = (0, 0), eqz = 0 == acc state6, rdm = 0, regMemAdd = 0, control = (control state6) {read_from_mem = False, write_to_mem = False, write_back_to_acc = False, write_back_to_pc = False}}
    finalState = if isHalt then state7 else cpu state7

fetch :: State -> State
fetch state = finalState
  where
    state1 = state {regMemAdd = pc state} -- salva PC no registrador REM
    state2 = memoryRead state1 -- Le REM da memoria e salva em RDM
    state3 = state2 {ireg = (rdm state2, 0)} -- Salva RDM na posicao alta de IReg
    state4 = state3 {regMemAdd = pc state3 + 1} -- incrementa pc para a prox linha da memoria, o parametro da instrucao
    state5 = memoryRead state4 -- Le REM da memoria e salva em RDM
    state6 = state5 {ireg = (fst (ireg state5), rdm state5)} -- Salva RDM na posicao baixa de IReg
    finalState = state6 {regMemAdd = pc state6 + 1} -- incrementa PC para o proximo ciclo

decode :: State -> State
decode state
  | ins == sto = state {control = (control state) {write_to_mem = True}}
  | ins == lod || ins == cpe || ins == add || ins == sub = state {control = (control state) {write_back_to_acc = True, read_from_mem = True}}
  | ins == jmp = state {control = (control state) {write_back_to_pc = True}}
  | ins == jmz = state {control = (control state) {write_back_to_pc = eqzFlag}}
  | otherwise = state
  where
    ins = fst (ireg state)
    eqzFlag = eqz state

memory :: State -> State
memory state
  | read_from_mem (control state) = memoryRead state
  | write_to_mem (control state) = memoryWrite state
  | otherwise = state

execute :: State -> State
execute state
  | ins == cpe = state {acc = if rdm state == acc state then 0 else 1}
  | ins == add = state {acc = rdm state + acc state}
  | ins == sub = state {acc = rdm state - acc state}
  | otherwise = state
  where
    ins = fst (ireg state)

writeBack :: State -> State
writeBack state
  | write_back_to_pc (control state) = state {pc = snd (ireg state)}
  | otherwise = state

{-- CONTROLADOR DE MEMORIA --}
memoryRead :: State -> State
memoryRead state = state {rdm = selectAddress (regMemAdd state) (mem state)}

memoryWrite :: State -> State
memoryWrite state = state {mem = persistAddress (regMemAdd state) (rdm state) (mem state)}

selectAddress :: Address -> Mem -> Content
selectAddress _ [] = 0
selectAddress address (item : memory)
  | fst item == address = snd item
  | otherwise = selectAddress address memory

persistAddress :: Address -> Content -> Mem -> Mem
persistAddress _ _ [] = []
persistAddress address content (item : memory)
  | fst item == address = (address, content) : memory
  | otherwise = item : persistAddress address content memory
