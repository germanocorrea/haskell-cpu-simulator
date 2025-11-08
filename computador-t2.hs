import Control.Monad
import Debug.Trace
import Distribution.Compat.Prelude (readMaybe)
import System.Environment
import System.IO

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
    write_back_to_pc :: Signal,
    finish :: Signal
  }
  deriving (Show)

-- rem como rem' pq rem já existe no prelude
data State = State {mem :: Mem, pc :: IC, ireg :: IReg, acc :: ACC, eqz :: EQZ, rdm :: RDM, rem' :: REM, control :: ControlSignals} deriving (Show)

lod = 2

sto = 4

jmp = 6

jmz = 8

cpe = 10

add = 14

sub = 16

nop = 18

hcf = 20

initialState :: State
initialState =
  State
    { mem = [],
      pc = 0, -- O contador de instruções é inicializado com zero
      ireg = (0, 0),
      acc = 0,
      eqz = True, -- EQZ indica se o acumulador é igual a zero
      rdm = 0,
      rem' = 0,
      control =
        ControlSignals
          { read_from_mem = False,
            write_to_mem = False,
            write_back_to_acc = False,
            write_back_to_pc = False,
            finish = False
          }
    }

main = do
  args <- getArgs
  progAsMem <- parseAsMem (head args)
  let progAsMem = progAsMem
  let finalState = cpu initialState {mem = progAsMem}
  let (a, b, c, d, e) = selectVideoMem finalState
  putStr (show a)
  putStr " "
  putStr (show b)
  putStr " "
  putStr (show c)
  putStr " "
  putStr (show d)
  putStr " "
  putStr (show e)
  putStrLn ""

parseAsMem :: String -> IO Mem
parseAsMem filename = do
  contents <- readFile filename
  let memoria = fromFileToMem (lines contents) 0
  return memoria

selectVideoMem :: State -> (Content, Content, Content, Content, Content)
selectVideoMem state =
  ( selectAddress 251 (mem state),
    selectAddress 252 (mem state),
    selectAddress 253 (mem state),
    selectAddress 254 (mem state),
    selectAddress 255 (mem state)
  )

fromFileToMem :: [String] -> Address -> Mem
fromFileToMem [] _ = []
fromFileToMem (line : lines) address = memItem ++ fromFileToMem lines (address + 1)
  where
    processed = processFileLine line
    memItem = maybe [] (: []) processed

processFileLine :: String -> Maybe MemItem
processFileLine line = extractMemItem parts
  where
    parts = takeWhile (/= "//") (words line)

extractMemItem :: [String] -> Maybe MemItem
extractMemItem [] = Nothing
extractMemItem [_] = Nothing
extractMemItem (addrString : (contString : _)) = do
  addr <- readMaybe addrString
  cont <- readMaybe contString
  return (addr, cont)

-- FETCH, DECODE, EXECUTE, MEMORY ACCESS, WRITE BACK
-- Memory antes de execute????
cpu :: State -> State
cpu state = finalState
  where
    cleanState = state {ireg = (0, 0), eqz = 0 == acc state, rdm = 0, rem' = 0, control = (control state) {read_from_mem = False, write_to_mem = False, write_back_to_acc = False, write_back_to_pc = False, finish = False}}
    state1 = fetch cleanState
    state2 = decode state1
    state3 = state2 {rem' = snd (ireg state2), rdm = acc state2}
    state4 = memory state3
    state5 = execute state4
    state6 = writeBack state5
    finalState = if finish (control state6) then state6 else cpu state6

fetch :: State -> State
fetch state = finalState
  where
    state1 = state {rem' = pc state} -- salva PC no registrador REM
    state2 = memoryRead state1 -- Le REM da memoria e salva em RDM
    state3 = state2 {ireg = (rdm state2, 0)} -- Salva RDM na posicao alta de IReg
    state4 = state3 {rem' = pc state3 + 1} -- incrementa pc para a prox linha da memoria, o parametro da instrucao
    state5 = memoryRead state4 -- Le REM da memoria e salva em RDM
    state6 = state5 {ireg = (fst (ireg state5), rdm state5)} -- Salva RDM na posicao baixa de IReg
    finalState = state6 {pc = pc state6 + 2} -- incrementa PC para o proximo ciclo

decode :: State -> State
decode state
  | ins == sto = state {control = (control state) {write_to_mem = True}}
  | ins == cpe || ins == add || ins == sub = state {control = (control state) {read_from_mem = True}} -- apesar de escreverem em ACC, a ULA faz isso direto pois ela possui conexao direta
  | ins == lod = state {control = (control state) {write_back_to_acc = True, read_from_mem = True}}
  | ins == jmp = state {control = (control state) {write_back_to_pc = True}}
  | ins == jmz = state {control = (control state) {write_back_to_pc = eqzFlag}}
  | ins == hcf = state {control = (control state) {finish = True}}
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
  | ins == add = state {acc = truncateDecimal (rdm state + acc state)}
  | ins == sub = state {acc = truncateDecimal (rdm state - acc state)}
  | otherwise = state
  where
    ins = fst (ireg state)

writeBack :: State -> State
writeBack state
  | write_back_to_pc (control state) = state {pc = snd (ireg state)}
  | write_back_to_acc (control state) = state {acc = rdm state}
  | otherwise = state

{-- CONTROLADOR DE MEMORIA --}
memoryRead :: State -> State
memoryRead state = state {rdm = selectAddress (rem' state) (mem state)}

memoryWrite :: State -> State
memoryWrite state = state {mem = persistAddress (rem' state) (rdm state) (mem state)}

selectAddress :: Address -> Mem -> Content
selectAddress _ [] = 0
selectAddress address (item : memory)
  | fst item == address = snd item
  | otherwise = selectAddress address memory

persistAddress :: Address -> Content -> Mem -> Mem
persistAddress address content [] = [(address, content)]
persistAddress address content (item : memory)
  | fst item == address = (address, content) : memory
  | otherwise = item : persistAddress address content memory

truncateDecimal :: Int -> Int
truncateDecimal x
  | x > 255 = 0
  | otherwise = x
