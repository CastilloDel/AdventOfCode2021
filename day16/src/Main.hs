import Data.Bifunctor (first, second)
import Data.Char (digitToInt)

main :: IO ()
main = do
  (testPacket, _) <- readInput "day16/test_input"
  (packet, _) <- readInput "day16/input"
  print $ "Test input: " ++ show (firstProblem testPacket) ++ " == 20"
  print $ "Problem input: " ++ show (firstProblem packet) ++ " == 989"
  print $ "Test input: " ++ show (secondProblem testPacket) ++ " == 1"
  print $ "Problem input: " ++ show (secondProblem packet) ++ " == 7936430475134"
  where
    readInput file = parseHexPacket <$> readFile file

data Packet = Packet {version :: Int, content :: PacketContent}

type PacketPair = (Packet, Packet)

data PacketContent
  = Sum [Packet]
  | Product [Packet]
  | Minimum [Packet]
  | Maximum [Packet]
  | Value Int
  | GreaterThan PacketPair
  | LessThan PacketPair
  | EqualTo PacketPair

parseHexPacket :: String -> (Packet, Int)
parseHexPacket = parsePacket . hexToBinary

parsePacket :: String -> (Packet, Int)
parsePacket binary = (packet, bitLength)
  where
    packet = Packet {version = version, content = content}
    bitLength = contentBitLength + 3
    version = binaryToInt $ take 3 binary
    (content, contentBitLength) = parsePacketContent $ drop 3 binary

parsePacketContent :: String -> (PacketContent, Int)
parsePacketContent binary = second (+ 3) packetContent
  where
    packetContent = parser rest
    parser =
      case typeID of
        1 -> first Product . parsePacketList
        2 -> first Minimum . parsePacketList
        3 -> first Maximum . parsePacketList
        4 -> first Value . parseValue
        5 -> first GreaterThan . parsePacketPair
        6 -> first LessThan . parsePacketPair
        7 -> first EqualTo . parsePacketPair
        _ -> first Sum . parsePacketList
    typeID = binaryToInt $ take 3 binary
    rest = drop 3 binary

parsePacketPair :: String -> (PacketPair, Int)
parsePacketPair s = case parsePacketList s of
  ([p1, p2], i) -> ((p1, p2), i)
  _ -> error $ "Invalid Pair: " ++ s

parsePacketList :: String -> ([Packet], Int)
parsePacketList binary = second (+ 1) packetContent
  where
    packetContent =
      if head binary == '0'
        then parsePacketListLength $ tail binary
        else parsePacketListNumber $ tail binary

parseValue :: String -> (Int, Int)
parseValue ('0' : s) = (binaryToInt $ take 4 s, 5)
parseValue (x : s) = (actualValue * magnitude + nextVal, 5 + nextLength)
  where
    -- Multiplying by the magnitude gives each block its correct value
    magnitude = 16 ^ (nextLength `div` 5)
    actualValue = binaryToInt $ take 4 s
    (nextVal, nextLength) = parseValue $ drop 4 s
parseValue _ = error "Error while parsing"

parsePacketListLength :: String -> ([Packet], Int)
parsePacketListLength binary = (packets, len + 15)
  where
    len = binaryToInt $ take 15 binary
    packetsBinary = drop 15 binary
    packets = parsePackets [] len packetsBinary
    parsePackets packets 0 binary = reverse packets
    parsePackets packets remaining binary =
      parsePackets (packet : packets) (remaining - len) $ drop len binary
      where
        (packet, len) = parsePacket binary

parsePacketListNumber :: String -> ([Packet], Int)
parsePacketListNumber binary = (packets, len + 11)
  where
    number = binaryToInt $ take 11 binary
    packetsBinary = drop 11 binary
    (packets, len) = parsePackets [] number packetsBinary
    parsePackets packets 0 binary = (reverse packets, 0)
    parsePackets packets remaining binary =
      second (+ newLen) $ parsePackets newPackets (remaining - 1) newBinary
      where
        newPackets = packet : packets
        newBinary = drop newLen binary
        (packet, newLen) = parsePacket binary

hexToBinary :: String -> String
hexToBinary = foldr foldIntoBinary ""
  where
    foldIntoBinary hexDigit acc = hexDigitToBinary hexDigit ++ acc

hexDigitToBinary :: Char -> String
hexDigitToBinary '0' = "0000"
hexDigitToBinary '1' = "0001"
hexDigitToBinary '2' = "0010"
hexDigitToBinary '3' = "0011"
hexDigitToBinary '4' = "0100"
hexDigitToBinary '5' = "0101"
hexDigitToBinary '6' = "0110"
hexDigitToBinary '7' = "0111"
hexDigitToBinary '8' = "1000"
hexDigitToBinary '9' = "1001"
hexDigitToBinary 'A' = "1010"
hexDigitToBinary 'B' = "1011"
hexDigitToBinary 'C' = "1100"
hexDigitToBinary 'D' = "1101"
hexDigitToBinary 'E' = "1110"
hexDigitToBinary 'F' = "1111"
hexDigitToBinary c = error "Invalid hex digit" ++ show c

binaryToInt :: String -> Int
binaryToInt = foldl (\acc digit -> acc * 2 + digitToInt digit) 0

firstProblem :: Packet -> Int
firstProblem (Packet version content) =
  version + sum (map firstProblem $ getSubPackets content)

getSubPackets :: PacketContent -> [Packet]
getSubPackets (Sum a) = a
getSubPackets (Product a) = a
getSubPackets (Minimum a) = a
getSubPackets (Maximum a) = a
getSubPackets (Value a) = []
getSubPackets (GreaterThan (a, b)) = [a, b]
getSubPackets (LessThan (a, b)) = [a, b]
getSubPackets (EqualTo (a, b)) = [a, b]

secondProblem :: Packet -> Int
secondProblem (Packet _ (Sum packets)) = sum $ map secondProblem packets
secondProblem (Packet _ (Product packets)) = product $ map secondProblem packets
secondProblem (Packet _ (Minimum packets)) = minimum $ map secondProblem packets
secondProblem (Packet _ (Maximum packets)) = maximum $ map secondProblem packets
secondProblem (Packet _ (Value val)) = val
secondProblem (Packet _ (GreaterThan (p1, p2))) =
  boolToInt $ secondProblem p1 > secondProblem p2
secondProblem (Packet _ (LessThan (p1, p2))) =
  boolToInt $ secondProblem p1 < secondProblem p2
secondProblem (Packet _ (EqualTo (p1, p2))) =
  boolToInt $ secondProblem p1 == secondProblem p2

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0