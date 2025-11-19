import io.StdIn._
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val nbmachine = 5
    val stock_cafe = Array.fill(nbmachine)(50)
    val stock_sucre = Array.fill(nbmachine)(30)
    val stock_lait = Array.fill(nbmachine)(500)
    val code_pins = Array.fill(nbmachine)("434343")
    var mode = 0
    var machineId = 0
    do {
      mode = 0
      var test = false
      do {
        println("\n\n        Nospresso Café")
        println("Veuillez sélectionner votre mode : ")
        println("1) Client")
        println("2) Admin")
        println("3) Quitter")
        print(">")
        mode = readInt()
        if (mode > 0 && mode < 4) {
          test = true
        } else {
          println("Choix Incorrect !")
        }
      } while (!test)
      if (mode != 3) {
        var redo = true
        do {
          do {
            print("Machine sélectionnée (1-5) > ")
            machineId = readInt()
            machineId -= 1
          } while (machineId < 0 || machineId > 4)
          if (mode == 1) {
            if (serveClient(machineId, stock_cafe, stock_sucre, stock_lait)) {
              redo = false
            }
          } else if (mode == 2) {
            redo = false
            if(validatePin(machineId, code_pins)) {
              var choix = 0
              do {
                println("Que voulez vous faire ?")
                println("1) Remplir les stocks")
                println("2) Changer le code PIN")
                print(">")
                choix = readInt()
              }while(choix<1||choix>2)
              if(choix == 1){
                restockMachine(machineId, stock_cafe, stock_sucre, stock_lait)
              }else if(choix == 2){
                updatePin(machineId, code_pins)
              }

            }else{
              mode = 3
            }
          }
        }while(redo == true)
      }
    }while(mode!=3)
  }

  def serveClient(machineId: Int, stock_cafe: Array[Int], stock_sucre: Array[Int], stock_lait: Array[Int]): Boolean = {
    var boisson = 0
    var test = false
    do {
      println("Veuillez sélectionner votre boisson :")
      println("1) Expresso - CHF 2.00")
      println("2) Cappuccino - CHF 2.50")
      println("3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)")
      print(">")
      boisson = readInt()
      if (boisson > 0 && boisson < 4) {
        test = true
      } else {
        println("Choix Incorrect !")
      }
    } while (!test)
    var taille = 0
    if (boisson == 3) {
      test = false
      do {
        println("Quelle taille voulez-vous ?")
        println("1) Petit")
        println("2) Moyen")
        println("3) Grand")
        print(">")
        taille = readInt()
        if (taille > 0 && taille < 4) {
          test = true
        } else {
          println("Choix Incorrect !")
        }
      } while (!test)
    }
    var sucre = 0
    test = false
    do {
      println("Souhaitez-vous ajouter du sucre ?")
      println("1) Sans sucre")
      println("2) Peu (5g) - CHF 0.10")
      println("3) Moyen (10g) - CHF 0.20")
      println("4) Beaucoup (15g) - CHF 0.30")
      print(">")
      sucre = readInt()
      if (sucre > 0 && sucre < 5) {
        test = true
      } else {
        println("Choix Incorrect !")
      }
    } while (!test)
    var doses = 0
    if (boisson == 2 || boisson == 3) {
      var lait = 0
      test = false
      do {
        println("Souhaitez-vous ajouter du lait en supplément ?")
        println("1) Oui")
        println("2) Non")
        print(">")
        lait = readInt()
        if (lait == 1 || lait == 2) {
          test = true
        } else {
          println("Choix Incorrect !")
        }
      } while (!test)
      if (lait == 1) {
        test = false
        do {
          println("Combien de dose ?")
          print(">")
          doses = readInt()
          if (doses > 0 && doses < 4) {
            test = true
          } else {
            println("Choix Incorrect")
          }
        } while (!test)
      }
    }
    // VERIFICATION DES STOCKS
    var cafe_use = 0
    var sucre_use = 0
    var lait_use = 0
    var prix = 0.0
    if (boisson == 1) {
      cafe_use = 8
      prix = 2
    } else if (boisson == 2) {
      cafe_use = 6
      lait_use = 100
      prix = 2.5
    } else if (taille == 1) {
      cafe_use = 6
      lait_use = 120
      prix = 2.7
    } else if (taille == 2) {
      cafe_use = 8
      lait_use = 150
      prix = 3.2
    } else {
      cafe_use = 12
      lait_use = 200
      prix = 3.7
    }
    if (sucre == 2) {
      sucre_use = 5
      prix = prix + 0.1
    } else if (sucre == 3) {
      sucre_use = 10
      prix = prix + 0.2
    } else if (sucre == 4) {
      sucre_use = 15
      prix = prix + 0.3
    }
    lait_use = lait_use + (50 * doses)
    var stock_ok = false
    if (cafe_use <= stock_cafe(machineId)) {
      if (lait_use <= stock_lait(machineId)) {
        if (sucre_use <= stock_sucre(machineId)) {
          stock_cafe(machineId) = stock_cafe(machineId) - cafe_use
          stock_lait(machineId) = stock_lait(machineId) - lait_use
          stock_sucre(machineId) = stock_sucre(machineId) - sucre_use
          stock_ok = true
        } else {
          println("Erreur : Quantité de sucre insuffisante pour npréparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
        }
      } else {
        println("Erreur : Quantité de lait insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson.")
      }
    } else {
      println("Erreur : Quantité de poudre de café insuffisante pour préparer la boisson sélectionnée. Veuillez choisir une autre boisson ou vérifier les stocks en mode Admin.")
    }
    if (stock_ok) {
      printf("Prix total : %.2f CHF\n", prix)
      println("Veuillez payer en utilisant TWINT.")
      var code_paiement = Random.alphanumeric.take(5).mkString
      println("Votre code de paiement est : " + code_paiement)
      println("(en attente de paiement...)")
      Thread.sleep(3000)
      println("\nPaiement confirmé !")
      println("Préparation de votre boisson ...")
      if (boisson == 1) {
        println("Votre Expresso est prêt ! Bonne dégustation !")
        return (true)
      } else if (boisson == 2) {
        println("Votre Cappuccino est prêt ! Bonne dégustation !")
        return (true)
      } else {
        println("Votre Latte est prêt ! Bonne dégustation !")
        return (true)
      }
    } else {
      return (false)
    }
  }

  def validatePin(machineId: Int, machinePins: Array[String]): Boolean = {
    var code = ""
    var fail = 0
    do {
      println("Entrez le code pin : ")
      code = readLine()
      if (code == machinePins(machineId)) {
        println("Mise à jour du code PIN pour la Machine 5.")
        return (true)
      } else {
        fail += 1
        println("Code PIN incorrect. "+(3-fail)+" tentatives restantes.")
        if(fail==3){
          print("\nTrop de tentatives échouées. Fin du programme.")
          return (false)
        }
      }
    }while(code!=machinePins(machineId))
    return (false)
  }

  def restockMachine(machineId: Int, stock_cafe: Array[Int],
                     stock_sucre: Array[Int], stock_lait: Array[Int]): Unit = {
    println("\nNiveaux de stock actuels:")
    println("Poudre de café : " + stock_cafe(machineId) + "g")
    println("Sucre : " + stock_sucre(machineId) + "g")
    printf("Lait : %.2fL\n",(stock_lait(machineId)).toFloat/1000)
    println("\nEntrez les quantités à ajouter :")
    var add_cafe = 0
    var add_lait = 0.0
    var add_sucre = 0
    do {
      print("Poudre de café > ")
      add_cafe = readInt()
    } while (add_cafe < 0)
    stock_cafe(machineId) = stock_cafe(machineId) + add_cafe
    do {
      print("Sucre > ")
      add_sucre = readInt()
    } while (add_sucre < 0)
    stock_sucre(machineId) = stock_sucre(machineId) + add_sucre
    do {
      print("Lait > ")
      add_lait = readDouble()
    } while (add_lait < 0)
    stock_lait(machineId) = stock_lait(machineId) + (add_lait*1000).toInt
    println("Les stocks ont été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
  def updatePin(machineId: Int, machinePins: Array[String]): Unit = {
    println("Mise à jour du code PIN pour la Machine "+(machineId+1)+".")
    var newPin = ""
    do{
      print("Entrez un nouveau code PIN à 6 chiffres > ")
      newPin = readLine()
    }while(!newPin.forall(Character.isDigit)||newPin.size!=6)
    machinePins(machineId) = newPin
    println("Le code PIN a été mis à jour avec succès.")
    println("Retour au menu principal...")
  }
}