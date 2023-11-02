object test5 extends App{
  
    object Config {
    val name = "Hello, "
        }
    
    val prefix = Config.name
    def greeting(name: String) {
    println(prefix + name)
    }

    val name = "Oleg"
    greeting(name)
}
