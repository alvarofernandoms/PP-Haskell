{-# LANGUAGE OverloadedStrings #-}

module HK2048 where

import Control.Monad.Random
import Control.Monad.Writer
import Data.Maybe
import Data.List
import System.IO

import qualified Data.Text as T

import System.Console.Haskeline

type Celula  = Maybe Int -- criacao de tipo Celula
type Linha   = [Celula] -- criacao de tipo Linha com Celula como parametro
type Matriz = [Linha] -- criacao de tipo Matriz com Linha como parametro
type Pontuacao = Int -- criacao de tipo Pontuacao do tipo Inteiro
data Direcao = Esquerda | Direita | Cima | Baixo deriving (Show, Eq) --definicao das direcoes dos movimentos

data Resultado = Derrota | Vitoria | Ativo | Invalido -- definicao dos estados de jogo
data ResultadoRodada = ResultadoRodada Pontuacao Resultado Matriz -- definicao do resultado a cada jogada

-- Funcao de formatacao da Matriz onde sera realizado o jogo
criarMatriz :: Matriz -> String
criarMatriz = T.unpack . T.unlines . fmap formatLinha
    where formatLinha = T.intercalate "|" . fmap (T.center 4 ' ' . formatCelula)
          formatCelula (Just x) = T.pack $ show x
          formatCelula _ = mempty

-- Funcao que realiza o calculo da soma dos numeros, caso realize a jogada certa
mudarLinha :: Linha -> Writer (Sum Pontuacao) Linha
mudarLinha linha = liftM (++ nothings) $ somaPares justs
    where (justs, nothings) = partition isJust linha
          somaPares (Just x:Just y:zs) | x == y = do
            let total = x + y
            tell $ Sum total
            rest <- somaPares zs
            return $ Just total : rest ++ [Nothing]
          somaPares (x:xs) = liftM (x :) $ somaPares xs
          somaPares [] = return []

-- Funcao que realiza a movimentacao dos numeros na matriz
mudarMatriz :: Direcao -> Matriz -> (Matriz, Sum Pontuacao)
mudarMatriz direcao = runWriter . case direcao of
    Direita  -> goDireita
    Esquerda  -> goEsquerda
    Cima -> liftM transpose . goDireita . transpose
    Baixo -> liftM transpose . goEsquerda . transpose
    where goDireita = mapM mudarLinha
          goEsquerda = mapM $ liftM reverse . mudarLinha . reverse

-- Funcao que verifica se a Matriz esta vazia
matrizVazia :: Int -> Matriz
matrizVazia n = replicate n $ replicate n Nothing

-- Funcao que verifica as coordenadas dos espaços vazios
disponivel :: Matriz -> [(Int, Int)]
disponivel = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices Nothing)

--  Funcao para atualizar matriz
atualizar :: Matriz -> (Int, Int) -> Celula -> Matriz
atualizar matriz (x, y) val = novaMatriz
    where (rs, r:rs') = splitAt x matriz
          (cs, _:cs') = splitAt y r
          novaLinha = cs <> (val : cs')
          novaMatriz = rs <> (novaLinha : rs')

-- Funcao para criar aleatoriamente o numero 2 ou numero 4 em alguma celula aleatoria da matriz
-- seguindo a regra de 90% de chances para o numero 2 e 10% de chance para o numero 4
inserirAleatorio :: MonadRandom m => Matriz -> m (Maybe Matriz)
inserirAleatorio matriz
    | null espacos = return Nothing
    | otherwise = do
        pos <- liftM (espacos !!) $ getRandomR (0, length espacos - 1)
        coin <- getRandomR (0 :: Float, 1)
        let novaCelula = Just $ if coin < 0.9 then 2 else 4
        return . Just $ atualizar matriz pos novaCelula
    where espacos = disponivel matriz

-- Funcao que verifica se a pontacao de alguma celula eh a pontuacao alvo
vencedor :: Celula -> Matriz -> Bool
vencedor ganhando = elem ganhando . concat

-- Funcao para verificar cada jogada
gameMovimento :: MonadRandom m => Celula -> Direcao -> Matriz -> m ResultadoRodada
gameMovimento goal direcao matriz =
    let (novaMatriz, Sum pontos) =
            mudarMatriz direcao matriz
        resultado = ResultadoRodada pontos
        mudou = matriz /= novaMatriz
    in if not mudou 
        then return $ if null $ disponivel novaMatriz
            then resultado Derrota novaMatriz
            else resultado Invalido matriz
        else if vencedor goal novaMatriz
            then return $ resultado Vitoria novaMatriz
            else do
                randoMatriz <- inserirAleatorio novaMatriz
                case randoMatriz of
                    Nothing -> return $ resultado Derrota novaMatriz
                    Just b  -> return $ resultado Ativo b


-- Funcao para solicitar ao jogador a entrada necessaria para realizar a jogada
-- realizar o calculo dos pontos
jogarGame :: Celula -> Matriz -> Int -> InputT IO ()
jogarGame goal matriz pontuacao = do
    liftIO . putStrLn $ criarMatriz matriz
    input <- getInputChar ""
    
    let direcao = case input of
         Just 'w' -> Just Cima
         Just 'a' -> Just Direita
         Just 's' -> Just Baixo
         Just 'd' -> Just Esquerda
         _ -> Nothing

    case direcao of
        Nothing ->
            jogarGame goal matriz pontuacao
        Just dir -> do
            ResultadoRodada pontos resultado novaMatriz <-
                liftIO $ gameMovimento goal dir matriz
            let totalPontuacao = pontos + pontuacao
            case resultado of
                Derrota -> liftIO $ 
                    putStrLn $ "Voce perdeu com " ++ show totalPontuacao ++ " pontos!"
                    
                Vitoria -> liftIO $ 
                    putStrLn $ "Voce ganhou com " ++ show totalPontuacao ++ " pontos!"
                Ativo -> do
                    liftIO $ do
                        putStrLn $ "Voce conseguiu " ++ show pontos ++ " pontos."
                        putStrLn $ "Pontuacao total: " ++ show totalPontuacao ++ " pontos.\n"
                    jogarGame goal novaMatriz totalPontuacao
                Invalido -> do
                    liftIO $ putStrLn "Movimento Invalidoo, tente novamente.\n"
                    jogarGame goal novaMatriz totalPontuacao


-- Funcao para iniciar o jogo
iniciarJogo :: MonadRandom m => Int -> m Matriz
iniciarJogo tam = do
    Just matriz  <- inserirAleatorio (matrizVazia tam)
    Just matriz' <- inserirAleatorio matriz
    return matriz'

-- Funcao main
mainGame :: IO ()
mainGame = do
    putStr "\nTamanho do tabuleiro: "
    op <- getLine
    let n = read op :: Int
    let tam = n
        goal = Just 2048

    iniciarMatriz <- iniciarJogo tam
    putStrLn "\n'w', 'a', 's', 'd'\n"
    runInputT defaultSettings $ jogarGame goal iniciarMatriz 0
