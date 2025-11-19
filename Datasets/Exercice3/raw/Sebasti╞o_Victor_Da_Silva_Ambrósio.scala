
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import scala.io.Source
import java.io.FileNotFoundException
import java.io.IOException
import scala.sys.exit

class Machine(idm:Int, pincodem:String, milkm:Int, sugarm:Int, coffeem:Int){
  var id = idm
  var pincode = pincodem
  var milk = milkm
  var sugar = sugarm
  var coffee = coffeem

  def addIngredient(ingredient: String, amount: Int): Unit ={
    if(ingredient == "milk"){
      milk += amount
    } else if(ingredient == "sugar"){
      sugar += amount
    } else if(ingredient == "coffee"){
      coffee += amount
    }
  }


  def removeIngredient(ingredient: String, amount: Int): Boolean={
    if(ingredient == "milk"){
      if(milk >= amount){
        milk -= amount
        return true
      }
    } else if(ingredient == "sugar"){
      if(sugar >= amount){
        sugar -= amount
        return true
      }

    } else if(ingredient == "coffee"){
      if(coffee >= amount){
        coffee -= amount
        return true

      }
    }
    return false
  }

}
object Main {

  var machines: ArrayBuffer[Machine] = ArrayBuffer()
  // Stocks initiaux
  var prixcafe = 0.0
  var prixsucre = 0.0
  var prixlait = 0.0



  //Exo 3
  def loadcsv(filename: String): ArrayBuffer[Machine]={
    var source: Source = null
    var tempsmachine: ArrayBuffer[Machine] = ArrayBuffer()

    try {
      source = Source.fromFile(filename)
      var lignefr = source.reset.getLines()
      lignefr = lignefr.drop(1)
      var ind = 1
      while (!lignefr.isEmpty){
        var ligne = lignefr.next()
        var lignedonnee = ligne.split(",")
        tempsmachine +=  new Machine(ind,lignedonnee(0),lignedonnee(1).toInt,lignedonnee(2).toInt,lignedonnee(3).toInt)
        ind += 1
      }
    } catch {
      case e: FileNotFoundException =>
        println(s"Erreur : Fichier '$filename' introuvable. Vérifiez le chemin.")
        exit(1)
      case e: IOException =>
        println(s"Erreur d'entrée/sortie lors de la lecture du fichier '$filename': ${e.getMessage}")
        exit(1)
      case e: NumberFormatException =>
        println(s"Erreur de conversion des données en nombres : ${e.getMessage}")
        exit(1)
      case e: Exception =>
        println(s"Erreur inattendue : ${e.getMessage}")
        exit(1)
    } finally {
      if (source != null) {
        source.close()
      }
    }
    return tempsmachine
  }

  def savecsv(filename: String, machines: ArrayBuffer[Machine]): Unit={
    var writer: PrintWriter = null
    try {
      writer = new PrintWriter(filename)
      println("Saving " + (machines.length).toString + " machines to machines.csv...")
      writer.println("PINCODE,MILK,SUGAR,COFFEE")
      for (x <- machines) {
        writer.println( x.pincode + "," + x.milk + "," + x.sugar+ "," + x.coffee)
      }
    } catch {
      case e: java.io.FileNotFoundException =>
        println(s"Erreur : Impossible d'ouvrir le fichier '$filename'. Vérifiez le chemin ou les permissions.")
        exit(1)
      case e: Exception =>
        println(s"Une erreur est survenue lors de l'écriture : ${e.getMessage}")
        exit(1)
    } finally {
      if (writer != null) {
        writer.close()
      }
      println("File save completed.")
    }

  }



  def validatePin(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var valide = false
    println("entrez le mot de passe : ")
    val test = scala.io.StdIn.readLine()
    for (x <- machines){
      if (x.id == machineId){
        if (test == x.pincode){
          valide = true
        }
      }
    }

    return valide
  }

  def updatePin(machineId: Int, machines: ArrayBuffer[Machine]): Unit ={
    var nouveauMotPass = ""
    println("Mise `a jour du code PIN pour la Machine " + machineId.toString)
    while(nouveauMotPass.length != 6){
      println("Entrez un nouveau code PIN `a 6 chiffres > ")
      nouveauMotPass = scala.io.StdIn.readLine()

    }
    for (x<- machines){
      if (x.id == machineId){
        x.pincode = nouveauMotPass
      }
    }
    println("Le code PIN a ete mis a jour avec succees. \nRetour au menu principal...")
  }

  def serveClient(machineId: Int, machines: ArrayBuffer[Machine]): Boolean = {
    var commandeValide = true

    // Choix de la boisson
    var choixBoisson = 0
    while (choixBoisson < 1 || choixBoisson > 3) {
      println("1. expresso (2.50 CHF)")
      println("2. cappuccino (2.70 CHF)")
      println("3. latte - 2.70 (petit), 3.20 (moyen), 3.70 (grand) ")
      print("> ")

      choixBoisson = scala.io.StdIn.readLine().toInt
      if (choixBoisson < 1 || choixBoisson > 3) {
        println("Choix de boisson invalide. Veuillez réessayer.")
      }
    }

    var cafeNecessaire = 0
    var laitNecessaire = 0

    if (choixBoisson == 1) { // Expresso
      cafeNecessaire = 8
      prixcafe = 2.50
    } else if (choixBoisson == 2) {
      cafeNecessaire = 6
      laitNecessaire = 100
      prixcafe = 2.70
    } else if (choixBoisson == 3) {
      var taille = 0
      while (taille < 1 || taille > 3) {
        println("1. Petit")
        println("2. Moyen")
        println("3. Grand")
        print("> ")

        taille = scala.io.StdIn.readLine().toInt
        if (taille == 1) {
          cafeNecessaire = 6
          laitNecessaire = 100
          prixcafe = 2.7
        } else if (taille == 2) {
          cafeNecessaire = 8
          laitNecessaire = 150
          prixcafe = 3.2
        } else if (taille == 3) {
          cafeNecessaire = 12
          laitNecessaire = 200
          prixcafe = 3.7
        } else {
          println("Choix de taille invalide. Veuillez réessayer.")
        }
      }
    }

    for (x <- machines){
      if(x.id == machineId){

        if (x.coffee < cafeNecessaire) {
          println("Stock insuffisant : pas assez de café.")
          return false
        }
        if (x.milk < laitNecessaire) {
          println("Stock insuffisant : pas assez de lait.")
          return false
        }

        x.coffee = x.coffee - cafeNecessaire
        x.milk = x.milk - laitNecessaire


        var choixSucre = 0
        var sucreNecessaire = 0

        while (choixSucre < 1 || choixSucre > 4) {
          println("1. pas de sucre")
          println("2. peu de sucre")
          println("3. moyen")
          println("4. beaucoup")
          print("> ")

          choixSucre = scala.io.StdIn.readLine().toInt

          if (choixSucre == 1) {
            prixsucre = 0.0
            sucreNecessaire = 0
          } else if (choixSucre == 2) {
            prixsucre = 0.10
            sucreNecessaire = 5
          } else if (choixSucre == 3) {
            prixsucre = 0.20
            sucreNecessaire = 10
          } else if (choixSucre == 4) {
            prixsucre = 0.30
            sucreNecessaire = 15
          } else {
            println("Choix invalide. Veuillez entrer un nombre entre 1 et 4.")
          }
        }


        if (x.sugar < sucreNecessaire) {
          println("Stock insuffisant : pas assez de sucre.")
          return false
        }
        x.sugar = x.sugar - sucreNecessaire


        var dosesLait = 0
        if (choixBoisson != 1) {
          var ajoutLait = 0


          while (ajoutLait != 1 && ajoutLait != 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("(Disponible uniquement pour Cappuccino et Latte)")
            println("1. Oui")
            println("2. Non")
            print("> ")

            ajoutLait = scala.io.StdIn.readLine().toInt

            if (ajoutLait != 1 && ajoutLait != 2) {
              println("Choix invalide.  Veuillez réessayer ")
            }
          }

          if (ajoutLait == 1) {

            while (dosesLait < 1 || dosesLait > 3) {
              println("Combien de doses ? (Max : 3)")
              print("> ")

              dosesLait = scala.io.StdIn.readLine().toInt

              if (dosesLait < 1 || dosesLait > 3) {
                println("Choix invalide. Vous pouvez ajouter entre 1 et 3 doses.")
              }
            }

            // Vérification du stock de lait
            val laitSupplementaire = dosesLait * 50
            if (x.milk < laitSupplementaire) {
              println("Stock insuffisant : pas assez de lait pour ajouter ces doses.")
            } else {
              x.milk = x.milk - laitSupplementaire
              prixlait = dosesLait * 0.05
              println("Vous avez ajouté " +dosesLait +" dose(s) de lait en supplément.")
            }
          } else {
            println("Aucun lait supplémentaire ajouté.")
          }
        }


        val prixtotal = prixcafe + prixsucre + prixlait

        var boisson = "rien"
        if (choixBoisson == 1) { boisson = "expresso" }
        if (choixBoisson == 2) { boisson = "cappuccino" }
        if (choixBoisson == 3) { boisson = "latte" }

        var sucree = "rien"
        if (choixSucre == 1) { sucree = "pas de sucre" }
        if (choixSucre == 2) { sucree = "peu de sucre - 0.10CHF " }
        if (choixSucre == 3) { sucree = "moyen de sucre - 0.20CHF" }
        if (choixSucre == 4) { sucree = "beaucoup de sucre - 0.30CHF" }

        var lait = "non"
        if (choixBoisson != 1 && dosesLait > 0) {
          if (dosesLait == 1) {
            lait = "oui, 1 dose"
          } else if (dosesLait == 2) {
            lait = "oui, 2 doses"
          } else if (dosesLait == 3) {
            lait = "oui, 3 doses"
          }
        }

        println("boisson selectionnée: " + boisson)
        println("Niveau de sucre: " + sucree)
        println("Lait supplémentaire: " + lait)
        println("Le prix de votre café est de : " + prixtotal.toFloat + " CHF")
        println("")
        println("Veuillez payer en utilisant Twint.")
        println("Votre code de paiement est :" + scala.util.Random.alphanumeric.take(5).mkString)
        println("(En attente de validation du paiement...)")
        Thread.sleep(5000)
        println("")
        println("paiement confirmé")
        println("Préparation de votre boisson...")
        Thread.sleep(3000)
        printf("Détail des prix : Café = %.2f\n", prixcafe )
        printf( "Lait = %.2f\n", prixlait )
        printf( "sucre = %.2f\n", prixsucre )


        println("Votre " + boisson + " est prêt ! Bonne dégustation.")
        return true
      }
    }


    return false
  }

  def restockMachine(machineId: Int, machines: ArrayBuffer[Machine]): Unit = {
    var aAjouterLait = 0
    var aAjouterSucre = 0
    var aAjoutercafee = 0

    for (x <- machines){
      if(x.id == machineId){
        println("Quantité de café à ajouter (g) :")
        aAjoutercafee = scala.io.StdIn.readLine().toInt
        if (aAjoutercafee > 0){
          x.coffee = x.coffee + aAjoutercafee
        }

        println("Quantité de sucre à ajouter (g) :")
        aAjouterSucre = scala.io.StdIn.readLine().toInt
        if (aAjouterSucre > 0){
          x.sugar = x.sugar + aAjouterSucre
        }

        println("Quantité de lait à ajouter (CL) :")
        aAjouterLait = scala.io.StdIn.readLine().toInt
        if (aAjouterLait > 0){
          x.milk = x.milk + aAjouterLait
        }
      }

    }
    println("Les stocks ont été mis à jour.")

  }



  def main(args: Array[String]): Unit = {
    var continuer = true
    println("Loading machines from machines.csv...")

    machines = loadcsv("machines.csv")

    for(x<- machines){

      println("Machine " + (x.id).toString + " loaded:")
      println("\t ID: " + (x.id).toString())
      println("\t Pincode: " + (x.pincode).toString())
      println("\t Milk: " + (x.milk).toString() + "ml")
      println("\t Sugar: " + (x.sugar).toString() + "g")
      println("\t Coffee: " + (x.coffee).toString() + "g")
      println("\n")
    }

    println("Successfully loaded " + (machines.length).toString + " machine(s).")

    while (continuer) {
      println("")
      println("Nospresso cafe")
      println("veuillez selectionner votre mode")
      println("1. modeclient")
      println("2. modeadmin")
      println("3. quitter")
      print("> ")

      val choix = scala.io.StdIn.readLine().toInt

      if (choix == 1) {
        modeClient()
      } else if (choix == 2) {
        continuer = modeAdmin()
      } else if (choix == 3) {
        println("Merci d'avoir utilisé Nospresso. Au revoir !")
        continuer = false
      } else {
        println("Choix invalide.")
      }
    }
    savecsv("machines.csv", machines)
  }

  def modeClient(): Unit = {

    var machineId = 6

    while (machineId > machines.length || machineId < 1){
      println("quelle machine voulez vous utiliser ?" +  "1" + "-" + (machines.length).toString())
      machineId = scala.io.StdIn.readLine().toInt
      if (machineId > machines.length || machineId < 1){
        println("erreur veuillez selectionner une machine entre 1 et 5 ")
      }
    }
    println("Machine selectionnee (1-5) > " + machineId)

    serveClient(machineId, machines)
  }

  def modeAdmin(): Boolean = {
    var test_pin = false
    var tentative_pin = 0
    var adminChoix = 0
    var machineId = 6
    var continuerAdmin = true
    var tentative_choix = 0

    while (machineId > machines.length || machineId < 1){
      println("quelle machine voulez vous utiliser ? " +  "1" + "-" + (machines.length).toString())
      machineId = scala.io.StdIn.readLine().toInt
      if (machineId > machines.length || machineId < 1){
        println("erreur veuillez selectionner une machine entre 1 et " + (machines.length).toString())
      }
    }
    println("Machine s´electionn´ee > " + machineId)

    while (test_pin == false){
      test_pin = validatePin(machineId, machines)
      if (test_pin == false){
        println("Code PIN incorrect. " + (2 - tentative_pin).toString + " tentatives restantes.")
      }
      tentative_pin = tentative_pin + 1
      if (tentative_pin >= 3){
        println("Trop de tentatives ´echou´ees. Fin du programme.")
        adminChoix = 3
        test_pin = true
        continuerAdmin = false
      }
    }

    while (adminChoix != 3) {
      println("1. Afficher stocks")
      println("2. Réapprovisionner stocks")
      println("3. Retour menu principal")
      println("4. Mettre a jour le pin")
      print("> ")

      adminChoix = scala.io.StdIn.readLine().toInt
      if (adminChoix == 1) {
        for (x <- machines){
          if (x.id == machineId){
            println(s"Sucre: " + x.sugar + "g")
            println(s"Café: " + x.coffee  + "g")
            println(s"Lait: " + x.milk + "CL")
          }
        }

      } else if (adminChoix == 2) {
        restockMachine(machineId, machines)
      } else if (adminChoix == 4) {
        updatePin(machineId, machines)
      }
      else if (adminChoix != 3) {
        println("Choix invalide.")
      }
    }

    continuerAdmin
  }
}