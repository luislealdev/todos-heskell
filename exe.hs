import System.IO
import Data.List
import Control.Monad
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar

-- Define la estructura de una Tarea
data Task = Task {
    category    :: String,
    name        :: String,
    description :: String,
    priority    :: Int,
    date        :: String
} deriving (Show, Read, Eq)

-- Función para pedir al usuario un String con un mensaje específico
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- Función para pedir al usuario un String no vacío con un mensaje específico
promptNonEmptyString :: String -> IO String
promptNonEmptyString text = do
    input <- prompt text
    if null input
        then do
            putStrLn "La entrada no puede estar vacía. Intente nuevamente."
            promptNonEmptyString text
        else return input

-- Función para pedir al usuario un String opcional con un mensaje específico
promptOptionalString :: String -> IO String
promptOptionalString text = do
    putStr text
    hFlush stdout
    getLine

-- Función para validar que la prioridad esté entre 1 y 5
validatePriority :: Int -> Bool
validatePriority p = p >= 1 && p <= 5

-- Función para pedir al usuario un Int con validación específica
promptValidInt :: String -> (Int -> Bool) -> IO Int
promptValidInt text validator = do
    putStr text
    hFlush stdout
    input <- getLine
    let parsedInput = reads input :: [(Int, String)]
    if null parsedInput
        then do
            putStrLn "Entrada inválida, por favor ingrese un número."
            promptValidInt text validator
        else do
            let (value, _) = head parsedInput
            if validator value
                then return value
                else do
                    putStrLn "Número fuera de rango. Intente nuevamente."
                    promptValidInt text validator

-- Función para validar el formato de la fecha (YYYY-MM-DD)
validateDate :: String -> Bool
validateDate date = case parseTimeM True defaultTimeLocale "%Y-%m-%d" date :: Maybe Day of
    Just _  -> True
    Nothing -> False

-- Función para pedir al usuario una fecha con validación
promptValidDate :: String -> IO String
promptValidDate text = do
    putStr text
    hFlush stdout
    input <- getLine
    if validateDate input
        then return input
        else do
            putStrLn "Fecha inválida, por favor use el formato YYYY-MM-DD."
            promptValidDate text

-- Función para crear una tarea pidiendo los datos al usuario
createTask :: IO Task
createTask = do
    category <- promptNonEmptyString "Ingrese la categoría de la tarea: "
    name <- promptNonEmptyString "Ingrese el nombre de la tarea: "
    description <- promptNonEmptyString "Ingrese la descripción de la tarea: "
    priority <- promptValidInt "Ingrese la prioridad de la tarea (1-5): " validatePriority
    date <- promptValidDate "Ingrese la fecha de la tarea (YYYY-MM-DD): "
    return (Task category name description priority date)

-- Función para guardar una tarea en un archivo
saveTaskToFile :: Task -> IO ()
saveTaskToFile task = do
    let filePath = "tasks.txt"
    let taskString = show task ++ "\n"
    appendFile filePath taskString
    putStrLn $ "Tarea guardada en " ++ filePath

-- Función para leer todas las tareas desde el archivo
readTasks :: IO [Task]
readTasks = do
    let filePath = "tasks.txt"
    content <- readFile filePath
    return (map read (lines content) :: [Task])

-- Función para buscar tareas por categoría
searchTasksByCategory :: String -> IO ()
searchTasksByCategory category = do
    tasks <- readTasks
    let filteredTasks = filter (\t -> category == Main.category t) tasks
    putStrLn "Tareas encontradas:"
    mapM_ print filteredTasks

-- Función para eliminar una tarea por su índice
deleteTaskByIndex :: IO ()
deleteTaskByIndex = do
    tasks <- readTasks
    putStrLn "Lista de tareas:"
    forM_ (zip [0..] tasks) $ \(i, task) -> do
        putStrLn $ show i ++ ": " ++ show task
    idx <- promptValidInt "Ingrese el número de la tarea a borrar: " (\i -> i >= 0 && i < length tasks)
    let filteredTasks = delete (tasks !! idx) tasks
    let filePath = "tasks.txt"
    writeFile filePath (unlines (map show filteredTasks))
    putStrLn "Tarea eliminada."

-- Función para mostrar todas las tareas
readTasksFromFile :: IO ()
readTasksFromFile = do
    tasks <- readTasks
    putStrLn "Lista de tareas:"
    mapM_ print tasks

-- Función para borrar todas las tareas del archivo
deleteTasksFromFile :: IO ()
deleteTasksFromFile = do
    let filePath = "tasks.txt"
    writeFile filePath ""
    putStrLn "Todas las tareas han sido borradas."

-- Función para editar una tarea por su índice
editTaskByIndex :: IO ()
editTaskByIndex = do
    tasks <- readTasks
    putStrLn "Lista de tareas:"
    forM_ (zip [0..] tasks) $ \(i, task) -> do
        putStrLn $ show i ++ ": " ++ show task
    idx <- promptValidInt "Ingrese el número de la tarea a editar: " (\i -> i >= 0 && i < length tasks)
    let task = tasks !! idx
    
    putStrLn $ "Editando tarea: " ++ show task
    
    newCategory <- promptOptionalString $ "Ingrese la nueva categoría de la tarea (presione Enter para mantener \"" ++ category task ++ "\"): "
    newName <- promptOptionalString $ "Ingrese el nuevo nombre de la tarea (presione Enter para mantener \"" ++ name task ++ "\"): "
    newDescription <- promptOptionalString $ "Ingrese la nueva descripción de la tarea (presione Enter para mantener \"" ++ description task ++ "\"): "
    newPriorityStr <- promptOptionalString $ "Ingrese la nueva prioridad de la tarea (presione Enter para mantener \"" ++ show (priority task) ++ "\"): "
    newDate <- promptOptionalString $ "Ingrese la nueva fecha de la tarea (presione Enter para mantener \"" ++ date task ++ "\"): "

    let newTask = Task {
            category = if null newCategory then category task else newCategory,
            name = if null newName then name task else newName,
            description = if null newDescription then description task else newDescription,
            priority = if null newPriorityStr then priority task else read newPriorityStr,
            date = if null newDate then date task else newDate
        }

    let updatedTasks = take idx tasks ++ [newTask] ++ drop (idx + 1) tasks
    let filePath = "tasks.txt"
    writeFile filePath (unlines (map show updatedTasks))
    putStrLn "Tarea actualizada."

-- Función para ordenar las tareas por fecha o prioridad
sortTasks :: IO ()
sortTasks = do
    putStrLn "Seleccione el criterio de ordenación:"
    putStrLn "1. Fecha de vencimiento (ascendente)"
    putStrLn "2. Fecha de vencimiento (descendente)"
    putStrLn "3. Prioridad (ascendente)"
    putStrLn "4. Prioridad (descendente)"
    option <- prompt "Opción: "
    tasks <- readTasks
    let sortedTasks = case option of
            "1" -> sortBy (\t1 t2 -> compare (parseDate (date t1)) (parseDate (date t2))) tasks
            "2" -> sortBy (\t1 t2 -> compare (parseDate (date t2)) (parseDate (date t1))) tasks
            "3" -> sortBy (\t1 t2 -> compare (priority t1) (priority t2)) tasks
            "4" -> sortBy (\t1 t2 -> compare (priority t2) (priority t1)) tasks
            _   -> tasks
    putStrLn "Tareas ordenadas:"
    mapM_ print sortedTasks
  where
    parseDate :: String -> Day
    parseDate str = case parseTimeM True defaultTimeLocale "%Y-%m-%d" str of
        Just day -> day
        Nothing  -> error "Fecha inválida en los datos de la tarea"

-- Función para mostrar el menú y manejar la selección del usuario
mainMenu :: IO ()
mainMenu = do
    putStrLn " "
    putStrLn "*----------------------------------------*"
    putStrLn "Seleccione una opción:"
    putStrLn "1. Crear nueva tarea"
    putStrLn "2. Leer todas las tareas"
    putStrLn "3. Buscar tareas por categoría"
    putStrLn "4. Borrar tarea"
    putStrLn "5. Borrar todas las tareas"
    putStrLn "6. Editar tarea"
    putStrLn "7. Ordenar tareas"
    putStrLn "8. Salir"
    putStrLn "*----------------------------------------*"
    option <- prompt "Opción: "
    case option of
        "1" -> do
            task <- createTask
            saveTaskToFile task
            mainMenu
        "2" -> do
            readTasksFromFile
            mainMenu
        "3" -> do
            category <- prompt "Ingrese la categoría a buscar: "
            searchTasksByCategory category
            mainMenu
        "4" -> do
            deleteTaskByIndex
            mainMenu
        "5" -> do
            deleteTasksFromFile
            mainMenu
        "6" -> do
            editTaskByIndex
            mainMenu
        "7" -> do
            sortTasks
            mainMenu
        "8" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida, intente nuevamente."
            mainMenu

-- Función principal
main :: IO ()
main = mainMenu
