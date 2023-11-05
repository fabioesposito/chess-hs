import Data.List (delete)

main :: IO ()
main = do
    putStrLn "hello"
    let game = initializeGame
    let p = getPiece (board game) (8,5)
    print p

data PieceType = King | Queen | Bishop | Knight | Rook | Pawn deriving (Show, Eq)
data PieceColor = White | Black deriving (Show, Eq)

data Piece = Piece {
    pieceType :: PieceType,
    pieceColor :: PieceColor
} deriving (Show, Eq)

type Position = (Int, Int)
type Board = [(Piece, Position)]

data GameState = GameState {
    board :: Board,
    turn :: PieceColor
} deriving (Show)

initializeGame :: GameState
initializeGame = GameState {
    board = [
        (Piece Rook Black, (1, 1)), (Piece Knight Black, (1, 2)), (Piece Bishop Black, (1, 3)), (Piece Queen Black, (1, 4)),
        (Piece King Black, (1, 5)), (Piece Bishop Black, (1, 6)), (Piece Knight Black, (1, 7)), (Piece Rook Black, (1, 8)),
        (Piece Rook White, (8, 1)), (Piece Knight White, (8, 2)), (Piece Bishop White, (8, 3)), (Piece Queen White, (8, 4)),
        (Piece King White, (8, 5)), (Piece Bishop White, (8, 6)), (Piece Knight White, (8, 7)), (Piece Rook White, (8, 8))
    ] ++ initializePawns White ++ initializePawns Black,
    turn = White
}

initializePawns :: PieceColor -> [(Piece, Position)]
initializePawns White = [(Piece Pawn White, (2, x)) | x <- [1..8]]
initializePawns Black = [(Piece Pawn Black, (7, x)) | x <- [1..8]]


isValidMove :: Piece -> Position -> Position -> Bool
isValidMove (Piece King _) (x1, y1) (x2, y2) = abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1
isValidMove (Piece Queen _) (x1, y1) (x2, y2) = x1 == x2 || y1 == y2 || abs (x2 - x1) == abs (y2 - y1)
isValidMove (Piece Bishop _) (x1, y1) (x2, y2) = abs (x2 - x1) == abs (y2 - y1)
isValidMove (Piece Knight _) (x1, y1) (x2, y2) = (abs (x2 - x1) == 1 && abs (y2 - y1) == 2) || (abs (x2 - x1) == 2 && abs (y2 - y1) == 1)
isValidMove (Piece Rook _) (x1, y1) (x2, y2) = x1 == x2 || y1 == y2
isValidMove (Piece Pawn _) (x1, y1) (x2, y2) = x2 == x1 + 1 && y2 == y1


movePiece :: GameState -> Position -> Position -> Maybe GameState
movePiece game from to
    | not (isValidMove piece from to) = Nothing
    | otherwise = Just $ GameState newBoard (nextTurn (turn game))
    where
        piece = getPiece (board game) from
        newBoard = moveInBoard (board game) from to

getPiece :: Board -> Position -> Piece
getPiece board position = piece
    where
        (piece, _) = head [x | x <- board, snd x == position]

moveInBoard :: Board -> Position -> Position -> Board
moveInBoard board from to = (piece, to) : delete (piece, from) board
    where
        piece = getPiece board from

nextTurn :: PieceColor -> PieceColor
nextTurn White = Black
nextTurn Black = White
