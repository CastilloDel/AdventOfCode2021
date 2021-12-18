import qualified Data.Bifunctor as Bifunctor
import Data.Char (digitToInt)
import Debug.Trace

main :: IO ()
main = do
  (testPacket, _) <- readInput "day16/test_input"
  (packet, _) <- readInput "day16/input"
  print $ "Test input: " ++ show (firstProblem testPacket) ++ " == 16"
  print $ "Problem input: " ++ show (firstProblem packet) ++ " == 989"
  where
    readInput file = parseHexPacket <$> readFile file

data Packet = Packet {version :: Int, content :: PacketContent}

data PacketContent
  = Value Int
  | SubPackets {typeID :: Int, packets :: [Packet]}

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
parsePacketContent binary =
  Bifunctor.second (+ 3) $
    if typeID == 4
      then parseValue rest
      else parseSubPackets typeID rest
  where
    typeID = binaryToInt $ take 3 binary
    rest = drop 3 binary

parseValue :: String -> (PacketContent, Int)
parseValue = Bifunctor.first Value . parseValue'
  where
    parseValue' (x : s) =
      if x == '0'
        then (val, 5)
        else
          let (nextVal, nextLength) = parseValue' $ drop 4 s
           in (val * 16 + nextVal, 5 + nextLength)
      where
        val = binaryToInt $ take 4 s
    parseValue' _ = error "Error while parsing"

parseSubPackets :: Int -> String -> (PacketContent, Int)
parseSubPackets typeID binary =
  Bifunctor.second
    (+ 1)
    ( if head binary == '0'
        then parseSubPacketsLength typeID $ tail binary
        else parseSubPacketsNumber typeID $ tail binary
    )

parseSubPacketsLength :: Int -> String -> (PacketContent, Int)
parseSubPacketsLength typeID binary =
  (SubPackets {typeID = typeID, packets = packets}, len + 15)
  where
    len = binaryToInt $ take 15 binary
    packetsBinary = drop 15 binary
    packets = parsePackets [] len packetsBinary
    parsePackets packets 0 binary = reverse packets
    parsePackets packets remaining binary =
      let (packet, len) = parsePacket binary
       in parsePackets (packet : packets) (remaining - len) $ drop len binary

parseSubPacketsNumber :: Int -> String -> (PacketContent, Int)
parseSubPacketsNumber typeID binary =
  (SubPackets {typeID = typeID, packets = packets}, len + 11)
  where
    number = binaryToInt $ take 11 binary
    packetsBinary = drop 11 binary
    (packets, len) = parsePackets [] number packetsBinary
    parsePackets packets 0 binary = (reverse packets, 0)
    parsePackets packets remaining binary =
      let (packet, newLen) = parsePacket binary
       in Bifunctor.second (+ newLen) $
            parsePackets (packet : packets) (remaining - 1) $
              drop newLen binary

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
firstProblem Packet {version = v, content = SubPackets {packets = ps}} =
  v + sum (map firstProblem ps)
firstProblem Packet {version = v} = v