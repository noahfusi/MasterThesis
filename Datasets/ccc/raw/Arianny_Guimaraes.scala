import java.io.{FileWriter, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source._
import scala.io.StdIn._
import scala.util.{Random, Try}


class Machine(val id: Int, var pincode: String, var milk: Int,
              var sugar: Int, var coffee: Int) {
  def addIngredient(ingredient: String, amount: Int): Unit = {
    if (ingredient == "milk"){
      milk += amount
    } else if (ingredient == "sugar"){
      sugar += amount
    } else if (ingredient == "coffee"){
      coffee += amount
    } else {
      println("pction invalide. Resseyée encore")
      return
    }
  }
  def removeIngredient(ingredient: String, amount: Int): Boolean = {
    if (ingredient == "milk" && milk >= amount){
      milk -= amount
      true
    } else if (ingredient == "sugar" && sugar >= amount){
      sugar -= amount
      true
    } else if (ingredient == "coffee" && coffee >= amount){
      coffee -= amount
      true
    } else {
      println("La quantite c'est insufficient sur le stock ou l'ingredient c'est invalide.")
      false
    }
  }
}//fecha a classe Machine 

object NospressoCafe {
  val machines: ArrayBuffer[Machine] = ArrayBuffer.empty

  def loadcsv(filename: String): ArrayBuffer[Machine] = {
    try {
      val fr = fromFile(filename)
      val lines = fr.reset.getLines().drop(1)
      val classe = new ArrayBuffer[Machine]()
      var i = 0
      while (!lines.isEmpty) {
        val line = lines.next()
        val parts = line.split(",")
        val pin = parts(0)
        val milk = parts(1).toInt
        val sugar = parts(2).toInt
        val coffee = parts(3).toInt
        classe += new Machine(i + 1, pin, milk, sugar, coffee)
        i += 1
      }
      return classe
    } catch {
      case ex: java.io.FileNotFoundException =>
        println(" Erreur : Fichier introuvable. Vérifiez le chemin d’accès et réessayez.")
        return null
    }
  }


  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit = {
    val result = Try {
      val writer = new PrintWriter(new FileWriter(filename))
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      machines.foreach { m =>
        writer.println(s"${m.pincode},${m.milk},${m.sugar},${m.coffee}")
      }
      writer.close()
    }

    if (result.isSuccess) {
      println(" Fichier sauvegardé avec succées..")
    } else {
      println(s"Error saving file: ${result.failed.get.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    val filename = "machines.csv"
    machines ++= loadcsv(filename)

    var running = true
    while (running) {
      println("Nespresso")
      println("Choissisez votre mode:")
      println("1) serveClient")
      println("2) ModeAdmin")
      println("3) ModeQuitter")
      println("> ")
      val mode = readLine()

      // choix du menu

      if (mode == "1") {
        serveClient ()
      } else if (mode == "2") {
        ModeAdmin ()
      } else if (mode == "3") {
        savecsv(filename, machines)
        println("Sauvegarde de machines dans machines.csv \n Fichier sauvegarde avec success")
        running = false
      } else {
        println("réessayer")
      }
    }
  }


  // Mode Client 
  def serveClient(): Unit = {
    println("Machine sélectionée  (1-" + machines.length + ")")
    val machineId = readLine().toInt - 1
    if (machineId >=0 && machineId < machines.length){
      println("Quelle boisson aimeriez boire:")
      println("1) Expresso 2,00")
      println("2) Cappuccino 2.50")
      println("3) Latte 2.70 (Petit) , 3.20 (Moyen), 3.70 (Grand) ")
      println(">")
      val BebidaEscolhida = readLine()
      if (BebidaEscolhida == "1"){
        Bebida(machineId, "Espresso", 8, 0, 2.0)
      } else if (BebidaEscolhida == "2"){
        Bebida(machineId, "Cappuccino", 6, 100, 2.5)
      } else if (BebidaEscolhida == "3"){
        Bebida(machineId, "Latte", 6, 120, 2.7)
      } else {
        println("Réessayer")
        return
      }
    }else{
      println("Le nombre de la machine c'est invalide. retour au début du menu.")
      return
    }

  }


  // Preparando Bebida 
  def Bebida(machineId: Int, nome: String, Cafe: Int, Lait: Int, Prix: Double): Boolean = {
    var var_cafe = Cafe
    var var_lait = Lait
    var var_prix = Prix

    //Selecionando o tamanho do Latte
    if (nome == "Latte"){
      println("Qual o tamanho do Latte?")
      println("1) Small - CHF 2.70")
      println("2) Medium - CHF 3.20")
      println("3) Large - CHF 3.70")
      println(">")
      val tailchoix= readLine()
      if (tailchoix == "1") {
        var_prix += 0.0
        var_cafe += 0
        var_lait  += 0
      } else if (tailchoix== "2") {
        var_prix += 0.5
        var_cafe += 2
        var_lait += 30
      } else if (tailchoix == "3") {
        var_prix += 1.0
        var_cafe += 6
        var_lait  += 80
      } else {
        println("Option non valable, reéssayer.")
        return false
      }
    }

    //Selecionando se haverá açucar ou não
    println("Vous voulliez sucre?? ")
    println("1) Sans sucre")
    println("2) Peu (5g) - 0.10")
    println("3) Moyen (10g)- CHF 0.20")
    println("4) Beaucoup (15g)- CHF 0.30")
    println(">")
    val sucrechoix = readLine()

    var quantitesucre = 0 
    if (sucrechoix == "1"){
      var_prix += 0.0
      quantitesucre = 0
    } else if (sucrechoix == "2"){
      var_prix += 0.1
      quantitesucre = 5
    } else if (sucrechoix == "3"){
      var_prix += 0.2
      quantitesucre = 10
    } else if (sucrechoix == "4"){
      var_prix += 0.3
      quantitesucre = 15
    } else {
      println("Option non valable, reéssayer")
      return false //para voltar no menu anterior sem prosseguir lendo as próximas linhas
    }

    //Pergunta se quer mais leite
    if (nome == "Cappuccino" || nome == "Latte"){
      println("Voulliez-vous extra lait?")
      println("(Seulement pour Cappuccino et Lait)")
      println("1) Oui")
      println("2) Non")
      println(">")
      val extralait = readLine()
      if (extralait == "2"){
        var_prix += 0.0
      } else if (extralait == "1"){
        println("Combien de doses? Le maximum de doses c'est 1, 2 ou 3")
        val doses = readLine()
        if (doses == "1" || doses == "2" || doses == "3"){
          //precisa converter pra inteiro pois doses é string
          var_prix += 0.05 * doses.toInt
          var_lait  += 50 * doses.toInt
        }else{
          println("La quantite c'est superieur à la valeur autorisée, Veulliez entrer un nombre valide de doses")
          return false
        }
      }else {
        println("Option non valable, réessayer.")
        return false
      }
    }

    if (machines(machineId).removeIngredient("coffee", var_cafe) &&
      machines(machineId).removeIngredient("milk", var_lait ) &&
      machines(machineId).removeIngredient("sugar", quantitesucre)){

      Payment(var_prix)
      println("Preparation de votre boisson")
      println("[...]")
      Thread.sleep(2000) // atendez 2 second
      println(s" Votre $nome est prête! Profiter ")
      return true

    } else {
      println("Error: Inssufissant la quantite pour faire votre boisson.")
      return false
    }
  }

  // mode Payment Twint
  def Payment(prixtotal: Double): Unit = {
    println(f"le prix total c'est:  CHF $prixtotal%.2f")
    val Twint = Random.alphanumeric.take(5).mkString
    println("Payer avec Twint")
    println(s"Payment avec le code Twint: $Twint")
    println("(Atendez le payment de validation...)")
    Thread.sleep(3000)
    println("Merci, Au revoir!.")
  }

  // Mode Administrador 
  def ModeAdmin(): Unit = {
    println("Machine sélectionée (1 to " + machines.length + "):")
    val machineId = readLine().toInt - 1
    if (machineId >= 0 && machineId < machines.length) {
      if (validatePin(machineId)) {
        println("Accès perméeable")
        println("Selectione une option:")
        println("1) Entrez le code PIN: ")
        println("2) Mise à jour le stock")
        val adminChoix = readLine()
        if (adminChoix == "1") {
          updatePin(machineId)
        } else if (adminChoix == "2") {
          restockMachine(machineId)
        } else {
          println("Accès refusé.")
          return
        }
      } else {
        println("Machine invalide. Returné au menu principal")
        return
      }
    }

    //Validar o PIN
    def validatePin(machineId: Int): Boolean = {
      var tentatives = 3
      while (tentatives > 0) {
        println("Entrez le code PIN: ")
        val saisiPin = scala.io.StdIn.readLine()
        if (saisiPin == machines(machineId).pincode) {
          return true
        } else {
          tentatives -= 1
          println(s"Iode Pin incorrect. Vous avez $tentatives restant.")
        }
      }
      println("Trop de tentatives échouées. Fin du programme.")
      return false
    }
  }
  // Atualizar PIN
  def updatePin(machineId: Int): Unit = {
    var validPin = false
    while (!validPin) {
      println(f"Mise à jour du code PIN pour la  ${machineId + 1}.")
      println("Invalid. Entrez un nouveau code PIN à 6 chiffres")
      val newPin = scala.io.StdIn.readLine()
      if (newPin.length == 6 && newPin.forall(_.isDigit)) {
        machines(machineId).pincode = newPin
        println("Le code PIN a été mis à jour avec succés.")
        validPin = true
      } else {
        println("Entrez un nouveau code PIN à 6 chiffres.")
      }
    }
    println("Le code PIN à etè mis à jour avec succèes.Retour au menu principal...")
    return
  }
  //Atualizando o estoque 
  def restockMachine(machineId: Int): Unit = {
    println("Niveaux de stock actuels::")
    println(f"Poudre de cafe: ${machines(machineId).coffee} g")
    println(f"Lait: ${machines(machineId).milk} ml")
    println(f"Sucre: ${machines(machineId).sugar} g")
    println("Entrez les quantiées à ajouter (Cafe, Lait et Sucre):")
    val ingredient = scala.io.StdIn.readLine()
    println("Entrez les quantiées à ajouter:")
    val amount = scala.io.StdIn.readLine().toInt
    machines(machineId).addIngredient(ingredient, amount)
    println("Stock niveaux mise à jour")
    println("Returner au menu...")
    return
  }
} //chave para fechar o objeto NospressoCafe