import io.StdIn._
import math._
import scala.util.Random

object Main {
  var stockdecafe = 50
  var stocksucre = 30
  var stocklait = 500

  def main(args: Array[String]): Unit = {
    var continuer = true
    do {

      //val modes
      var mode = 0
      val client = 1
      val admin = 2
      val quitter = 3

      //val twint
      val codetwint = 0


      //val stock


      val quantitecafeexpresso = 8
      val quantitesucreexpresso = 5
      val quantiteMsucreexpresso = 10
      val quantiteBCPxpresso = 15

      val quantitecafecappuccino = 6
      val quantitelaitcappuccino = 100
      val quantitecafepetitlatte = 6
      val quantitelaitpetitlatte = 120
      val quantitecafeMlatte = 8
      val quantitelaitMlatte = 150
      val quantitecafeGlatte = 12
      val quantitelaitGlatte = 200
      val quantite1dose = 50
      val quantite2dose = 100
      val quantite3dose = 150


      //val boisson
      var choixboisson = 0
      val expresso = 1
      val cappucino = 2
      val latte = 3
      val coutexpresso = 2
      val coutcappuccino = 2.5
      val coutlatte = 2.7
      val coutlattemoyen = 3.20
      val coutlattegrand = 3.70
      var couttot = 0
      var choixtaille = 0
      val petitlatte = 1
      val moyenlatte = 2
      val grandlatte = 3

      //val sucre
      var choixsucre = 0
      val sanssucre = 1
      val peudesucre = 2
      val moyendesucre = 3
      val bcpdesucre = 4


      val coutpeusucre = 0.10
      val coutmoyensucre = 0.20
      val coutbcpsucre = 0.30

      // val lait
      var choixlait = 0
      val sanslait = 0
      val dose1lait = 1
      val dose2lait = 2
      val dose3lait = 3
      val ouilait = 1
      val nonlait = 2
      var choixdoselait = 0
      val coutdose1lait = 0.05
      val coutdose2lait = 0.10
      val coutdose3lait = 0.15


      println("Nospresso Café")
      println("Veuillez sélectionner votre mode : ")
      println("1) Client ")
      println("2) Admin ")
      println("3) Quitter ")

      do {
        mode = readInt()
        if (mode != client && mode != admin && mode != quitter) {
          println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) Client" + "\n" + "2) Admin" + "\n" + "3) Quitter ")
        }

      } while (mode != client && mode != admin && mode != quitter)


      if (mode == client) {

        println("choisire une boisson")


        println("1)Expresso - CHF 2.00 " + "\n" + "2)Cappucino - CHF 2.50 " + "\n" + "3)Latte - CHF 2.70 (Petit ) , CHF 3.20 (Moyen) , CHF 3.70 (Grand) ")
        do {
          choixboisson = readInt()
          if (choixboisson != expresso && choixboisson != cappucino && choixboisson != latte) {
            println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) Expresso" + "\n" + "2) Cappucino" + "\n" + "3) Latte")
          }
        } while (choixboisson != expresso && choixboisson != cappucino && choixboisson != latte)

        if (choixboisson == expresso) {

          println("Souhaitez-vous ajouter du sucre ?" + "\n" + "1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")


          do {
            choixsucre = readInt()
            if (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + " 1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
            }
          } while (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre)

          if (choixsucre == sanssucre) {
            println("Boisson séleéctionnee : Expresso " + "\n"
              + "Niveau de sucre : Sans sucre " + "\n" + "Prix total: " + " CHF " + coutexpresso)
            if (stockdecafe >= quantitecafeexpresso) {
              stockdecafe -= quantitecafeexpresso
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Expresso est pret  ! " + " Bonne dégustation ! ")
            } else
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin.")

          } else if (choixsucre == peudesucre) {
            println("Boisson séleéctionnee : Expresso" + "\n"
              + "Niveau de sucre : Peu " + "\n" + "Prix total: " + " CHF " + coutexpresso + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutexpresso + coutpeusucre))
            if (stockdecafe >= quantitecafeexpresso && stocksucre >= quantitesucreexpresso) {
              stockdecafe -= quantitecafeexpresso
              stocksucre -= quantitesucreexpresso
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Expresso est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeexpresso) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

          }


          else if (choixsucre == moyendesucre) {
            println("Boisson séleéctionnee : Expresso" + "\n"
              + "Niveau de sucre : Moyen " + "\n" + "Prix total: " + " CHF " + coutexpresso + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (coutexpresso + coutmoyensucre))
            if (stockdecafe >= quantitecafeexpresso && stocksucre >= quantiteMsucreexpresso) {
              stockdecafe -= quantitecafeexpresso
              stocksucre -= quantiteMsucreexpresso
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Expresso est pret  ! " + " Bonne dégustation ! ")
            } else if (stockdecafe < quantitecafeexpresso) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boissonou ou verifier les stocks en mode Admin")
            } else {
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            }


          }
          else {
            println("Boisson séleéctionnee : Expresso" + "\n"
              + "Niveau de sucre : Beaucoup " + "\n" + "Prix total: " + " CHF " + coutexpresso + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutexpresso + coutbcpsucre))
            if (stockdecafe >= quantitecafeexpresso && stocksucre >= quantiteBCPxpresso) {
              stockdecafe -= quantitecafeexpresso
              stocksucre -= quantiteBCPxpresso
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Expresso est pret  ! " + " Bonne dégustation ! ")
            } else if (stockdecafe < quantitecafeexpresso) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boissonou ou verifier les stocks en mode Admin")
            } else {
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            }
          }

        }


        // CAPèPUCCINNo}


        else if (choixboisson == cappucino) {
          println("Souhaitez-vous ajouter du sucre ?" + "\n" + "1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
          do {
            choixsucre = readInt()
            if (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + " 1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
            }
          } while (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre)
          if (choixsucre == sanssucre) {
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")
            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n"
                + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire : Non " + "\n" + coutcappuccino + "CHF")
              if (stockdecafe >= quantitecafecappuccino) {
                stockdecafe -= quantitecafecappuccino
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")
              } else
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin.")
            }

            //PAIEMENT ECT


            else if (choixlait == ouilait) {
              println("Combien de dose ?")
              do {
                choixdoselait = readInt()
                if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3")
                }
              } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
              if (choixdoselait == dose1lait) {
                println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose1lait + " = " + " CHF " + (coutcappuccino + coutdose1lait))
                if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocklait >= quantite1dose) {
                  stockdecafe -= quantitecafecappuccino
                  stocklait -= quantitelaitcappuccino
                  stocklait -= quantite1dose
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafecappuccino) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else if (choixdoselait == dose2lait) {
                println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose2lait + " = " + " CHF " + (coutcappuccino + coutdose2lait))
                if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocklait >= quantite2dose) {
                  stockdecafe -= quantitecafecappuccino
                  stocklait -= quantitelaitcappuccino
                  stocklait -= quantite2dose
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafecappuccino) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              } else
                println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose3lait + " = " + " CHF " + (coutcappuccino + coutdose3lait))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocklait >= quantite3dose) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocklait -= quantite3dose
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            }

          } //sucre peu
          else if (choixsucre == peudesucre) {
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")

            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n"
                + "Niveau de sucre : Peu" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutcappuccino + coutpeusucre))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantitesucreexpresso) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocksucre -= quantitesucreexpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitcappuccino) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


            } else if (choixlait == ouilait)
              println("Combien de dose ?")
            do {
              choixdoselait = readInt()
              if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
              }

            } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
            if (choixdoselait == dose1lait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutcappuccino + coutdose1lait + coutpeusucre))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantitesucreexpresso && stocklait > quantite1dose) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocklait -= quantite1dose
                stocksucre -= quantitesucreexpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitcappuccino + quantite1dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


            } else if (choixdoselait == dose2lait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutcappuccino + coutdose2lait + coutpeusucre))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantitesucreexpresso && stocklait >= quantite2dose) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocksucre -= quantitesucreexpresso
                stocklait -= quantite2dose
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitcappuccino + quantite2dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            } else
              println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutcappuccino + coutdose3lait + coutpeusucre))
            if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantitesucreexpresso && stocklait >= quantite3dose) {
              stockdecafe -= quantitecafecappuccino
              stocklait -= quantitelaitcappuccino
              stocksucre -= quantitesucreexpresso
              stocklait -= quantite3dose
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafecappuccino) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else if (stocklait < quantitelaitcappuccino + quantite3dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            // sucre moyen

          } else if (choixsucre == moyendesucre) {
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n"
                + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (coutcappuccino + coutmoyensucre))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteMsucreexpresso) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocksucre -= quantiteMsucreexpresso

                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitcappuccino) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


            } else if (choixlait == ouilait)
              println("Combien de dose ?")
            do {
              choixdoselait = readInt()
              if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
              }

            } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
            if (choixdoselait == dose1lait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (coutcappuccino + coutdose1lait + coutmoyensucre))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteMsucreexpresso && stocklait >= quantite1dose) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocksucre -= quantiteMsucreexpresso
                stocklait -= quantite1dose
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitcappuccino + quantite1dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            } else if (choixdoselait == dose2lait) {
              println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (2.81))
              if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteMsucreexpresso && stocklait >= quantite2dose) {
                stockdecafe -= quantitecafecappuccino
                stocklait -= quantitelaitcappuccino
                stocksucre -= quantiteMsucreexpresso
                stocklait -= quantite2dose
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafecappuccino) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitcappuccino + quantite2dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            } else
              println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (coutcappuccino + coutdose3lait + coutmoyensucre))
            if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteMsucreexpresso && stocklait >= quantite3dose) {
              stockdecafe -= quantitecafecappuccino
              stocklait -= quantitelaitcappuccino
              stocksucre -= quantiteMsucreexpresso
              stocklait -= quantite3dose
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafecappuccino) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else if (stocklait < quantitelaitcappuccino + quantite3dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            // bcp de sucre
          } else
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


          do {
            choixlait = readInt()
            if (choixlait != ouilait && choixlait != nonlait) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
            }
          } while (choixlait != ouilait && choixlait != nonlait)
          if (choixlait == nonlait) {
            println("Boisson séleéctionnee : Cappuccino " + "\n"
              + "Niveau de sucre : Beaucoup" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutcappuccino + coutbcpsucre))
            if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteBCPxpresso) {
              stockdecafe -= quantitecafecappuccino
              stocklait -= quantitelaitcappuccino
              stocksucre -= quantiteBCPxpresso

              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Cappuccino est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafecappuccino) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else if (stocklait < quantitelaitcappuccino) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            //PAIEMENT ECT


          } else if (choixlait == ouilait)
            println("Combien de dose ?")
          do {
            choixdoselait = readInt()
            if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
            }

          } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
          if (choixdoselait == dose1lait) {
            println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (2.851))
            if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteBCPxpresso && stocklait >= quantite1dose) {
              stockdecafe -= quantitecafecappuccino
              stocklait -= quantitelaitcappuccino
              stocksucre -= quantiteBCPxpresso
              stocklait -= quantite1dose
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Cappucino est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafecappuccino) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else if (stocklait < quantitelaitcappuccino + quantite1dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

          } else if (choixdoselait == dose2lait) {
            println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutcappuccino + coutdose2lait + coutbcpsucre))
            if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteBCPxpresso && stocklait >= quantite2dose) {
              stockdecafe -= quantitecafecappuccino
              stocklait -= quantitelaitcappuccino
              stocksucre -= quantiteBCPxpresso
              stocklait -= quantite2dose
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Cappucino est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafecappuccino) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else if (stocklait < quantitelaitcappuccino + quantite2dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

          } else
            println("Boisson séleéctionnee : Cappuccino " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutcappuccino + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (2.95))
          if (stockdecafe >= quantitecafecappuccino && stocklait >= quantitelaitcappuccino && stocksucre >= quantiteBCPxpresso && stocklait >= quantite3dose) {
            stockdecafe -= quantitecafecappuccino
            stocklait -= quantitelaitcappuccino
            stocksucre -= quantiteBCPxpresso
            stocklait -= quantite3dose
            println("Veuillez payer en utilisant Twint.")

            // Génération d'une chaîne alphanumérique de 5 caractères
            val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
            val length = 5
            val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

            // Affichage du code Twint
            println(s"Votre code Twint : $codeTwint")
            println("En attente de paiement...")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a ete accepte.")
            println("preparation de votre boisson ")
            Thread.sleep(5000)
            println("Votre Cappucino est pret  ! " + " Bonne dégustation ! ")

          } else if (stockdecafe < quantitecafecappuccino) {
            println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
          } else if (stocklait < quantitelaitcappuccino + quantite3dose) {
            println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
          } else
            println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


        } else {
          println("Selectionnez une taille pour votre Latte " + "\n" + "1) Petit " + "\n" + "2) Moyen " + "\n" + "3) Grand ")
          do {
            choixtaille = readInt()

            if (choixtaille != petitlatte && choixtaille != moyenlatte && choixtaille != grandlatte) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) Petit" + "\n" + "2) Moyen " + "\n" + "3) Grand ")
            }
          } while (choixtaille != petitlatte && choixtaille != moyenlatte && choixtaille != grandlatte)

          if (choixtaille == petitlatte) {
            println("Souhaitez-vous ajouter du sucre ?" + "\n" + "1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
            do {
              choixsucre = readInt()
              if (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + " 1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
              }
            } while (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre)
            if (choixsucre == sanssucre) {
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")
              do {
                choixlait = readInt()
                if (choixlait != ouilait && choixlait != nonlait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
                }
              } while (choixlait != ouilait && choixlait != nonlait)
              if (choixlait == nonlait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n"
                  + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire : Non " + "\n" + coutlatte + "CHF")
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte

                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                }
              }

              else if (choixlait == ouilait) {
                println("Combien de dose ?")
                do {
                  choixdoselait = readInt()
                  if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                    println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3")
                  }
                } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
                if (choixdoselait == dose1lait) {
                  println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose1lait + " = " + " CHF " + (coutlatte + coutdose1lait))
                  if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite1dose) {
                    stockdecafe -= quantitecafepetitlatte
                    stocklait -= quantitelaitpetitlatte
                    stocklait -= quantite1dose
                    println("Veuillez payer en utilisant Twint.")

                    // Génération d'une chaîne alphanumérique de 5 caractères
                    val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                    val length = 5
                    val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                    // Affichage du code Twint
                    println(s"Votre code Twint : $codeTwint")
                    println("En attente de paiement...")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a ete accepte.")
                    println("preparation de votre boisson ")
                    Thread.sleep(5000)
                    println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                  } else if (stockdecafe < quantitecafepetitlatte) {
                    println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                  } else
                    println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

                } else if (choixdoselait == dose2lait) {
                  println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose2lait + " = " + " CHF " + (2.8))
                  if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite2dose) {
                    stockdecafe -= quantitecafepetitlatte
                    stocklait -= quantitelaitpetitlatte
                    stocklait -= quantite2dose
                    println("Veuillez payer en utilisant Twint.")

                    // Génération d'une chaîne alphanumérique de 5 caractères
                    val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                    val length = 5
                    val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                    // Affichage du code Twint
                    println(s"Votre code Twint : $codeTwint")
                    println("En attente de paiement...")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a ete accepte.")
                    println("preparation de votre boisson ")
                    Thread.sleep(5000)
                    println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                  } else if (stockdecafe < quantitecafepetitlatte) {
                    println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                  } else
                    println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

                } else
                  println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose3lait + " = " + " CHF " + (coutlatte + coutdose3lait))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite3dose) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocklait -= quantite3dose
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              } //sucre peu
            } else if (choixsucre == peudesucre) {
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")

              do {
                choixlait = readInt()
                if (choixlait != ouilait && choixlait != nonlait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
                }
              } while (choixlait != ouilait && choixlait != nonlait)
              if (choixlait == nonlait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n"
                  + "Niveau de sucre : Peu" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (2.8))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocksucre >= quantitesucreexpresso) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocksucre -= quantitesucreexpresso
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else if (stocklait < quantitecafepetitlatte) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


              } else if (choixlait == ouilait)
                println("Combien de dose ?")
              do {
                choixdoselait = readInt()
                if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
                }

              } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
              if (choixdoselait == dose1lait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutlatte + coutdose1lait + coutpeusucre))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite1dose && stocksucre >= quantitesucreexpresso) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocklait -= quantite1dose
                  stocksucre -= quantitesucreexpresso
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else if (stocklait < quantitelaitpetitlatte + quantite1dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              } else if (choixdoselait == dose2lait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (2.801))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite2dose && stocksucre >= quantitesucreexpresso) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocklait -= quantite2dose
                  stocksucre -= quantitesucreexpresso
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else if (stocklait < quantitelaitpetitlatte + quantite2dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              } else
                println("Boisson séleéctionnee : Latte (Petit)" + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutlatte + coutdose3lait + coutpeusucre))
              if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite1dose && stocksucre >= quantitesucreexpresso) {
                stockdecafe -= quantitecafepetitlatte
                stocklait -= quantitelaitpetitlatte
                stocklait -= quantite1dose
                stocksucre -= quantitesucreexpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafepetitlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitpetitlatte + quantite3dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              // sucre moyen

            } else if (choixsucre == moyendesucre) {
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


              do {
                choixlait = readInt()
                if (choixlait != ouilait && choixlait != nonlait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
                }
              } while (choixlait != ouilait && choixlait != nonlait)
              if (choixlait == nonlait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n"
                  + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (2.90))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocksucre >= quantiteMsucreexpresso) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocksucre -= quantiteMsucreexpresso
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else if (stocklait < quantitelaitpetitlatte) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


              } else if (choixlait == ouilait)
                println("Combien de dose ?")
              do {
                choixdoselait = readInt()
                if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
                }

              } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
              if (choixdoselait == dose1lait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (coutlatte + coutdose1lait + coutmoyensucre))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite1dose && stocksucre >= quantiteMsucreexpresso) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocklait -= quantite1dose
                  stocksucre -= quantiteMsucreexpresso
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else if (stocklait < quantitelaitpetitlatte + quantite1dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              } else if (choixdoselait == dose2lait) {
                println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.0))
                if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite2dose && stocksucre >= quantiteMsucreexpresso) {
                  stockdecafe -= quantitecafepetitlatte
                  stocklait -= quantitelaitpetitlatte
                  stocklait -= quantite2dose
                  stocksucre -= quantiteMsucreexpresso
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafepetitlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
                } else if (stocklait < quantitelaitpetitlatte + quantite2dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              } else
                println("Boisson séleéctionnee : Latte(Petit) " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total :" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.05))
              if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite3dose && stocksucre >= quantiteMsucreexpresso) {
                stockdecafe -= quantitecafepetitlatte
                stocklait -= quantitelaitpetitlatte
                stocklait -= quantite3dose
                stocksucre -= quantiteMsucreexpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafepetitlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitpetitlatte + quantite3dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

              // bcp de sucre
            } else
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Latte (Petit) " + "\n"
                + "Niveau de sucre : Beaucoup" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlatte + coutbcpsucre))
              if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocksucre >= quantiteBCPxpresso) {
                stockdecafe -= quantitecafepetitlatte
                stocklait -= quantitelaitpetitlatte
                stocksucre -= quantiteBCPxpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafepetitlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitpetitlatte) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


            } else if (choixlait == ouilait)
              println("Combien de dose ?")
            do {
              choixdoselait = readInt()
              if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
              }

            } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
            if (choixdoselait == dose1lait) {
              println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlatte + coutdose1lait + coutbcpsucre))
              if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite1dose && stocksucre >= quantiteBCPxpresso) {
                stockdecafe -= quantitecafepetitlatte
                stocklait -= quantitelaitpetitlatte
                stocklait -= quantite1dose
                stocksucre -= quantiteBCPxpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafepetitlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitpetitlatte + quantite1dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            } else if (choixdoselait == dose2lait) {
              println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlatte + coutdose2lait + coutbcpsucre))
              if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite2dose && stocksucre >= quantiteBCPxpresso) {
                stockdecafe -= quantitecafepetitlatte
                stocklait -= quantitelaitpetitlatte
                stocklait -= quantite2dose
                stocksucre -= quantiteBCPxpresso
                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafepetitlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
              } else if (stocklait < quantitelaitpetitlatte + quantite2dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")

            } else
              println("Boisson séleéctionnee : Latte (Petit) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlatte + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlatte + coutdose3lait + coutbcpsucre))
            if (stockdecafe >= quantitecafepetitlatte && stocklait >= quantitelaitpetitlatte && stocklait >= quantite3dose && stocksucre >= quantiteBCPxpresso) {
              stockdecafe -= quantitecafepetitlatte
              stocklait -= quantitelaitpetitlatte
              stocklait -= quantite3dose
              stocksucre -= quantiteBCPxpresso
              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafepetitlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une autre boisson ou ou verifier les stocks en mode Admin")
            } else if (stocklait < quantitelaitpetitlatte + quantite3dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir  une autre boisson ou verifier les stocks en mode Admin")


            // MOYEN LATTE

          } else if (choixtaille == moyenlatte) {
            println("Souhaitez-vous ajouter du sucre ?" + "\n" + "1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")

            do {
              choixsucre = readInt()
              if (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + " 1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
              }
            } while (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre)
            if (choixsucre == sanssucre) {
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")
              do {
                choixlait = readInt()
                if (choixlait != ouilait && choixlait != nonlait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
                }
              } while (choixlait != ouilait && choixlait != nonlait)
              if (choixlait == nonlait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n"
                  + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire : Non " + "\n" + coutlattemoyen + "CHF")
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte

                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée.Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée.Veuillez choisir une taille plus petite ou essayer une autre boisson")
                }


              }

              //PAIEMENT ECT


              else if (choixlait == ouilait) {
                println("Combien de dose ?")
                do {
                  choixdoselait = readInt()
                  if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                    println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3")
                  }
                } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
                if (choixdoselait == dose1lait) {
                  println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose1lait + " = " + " CHF " + (coutlattemoyen + coutdose1lait))
                  if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocklait >= quantite1dose) {
                    stockdecafe -= quantitecafeMlatte
                    stocklait -= quantitelaitMlatte
                    stocklait -= quantite1dose
                    println("Veuillez payer en utilisant Twint.")

                    // Génération d'une chaîne alphanumérique de 5 caractères
                    val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                    val length = 5
                    val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                    // Affichage du code Twint
                    println(s"Votre code Twint : $codeTwint")
                    println("En attente de paiement...")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a ete accepte.")
                    println("preparation de votre boisson ")
                    Thread.sleep(5000)
                    println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                  } else if (stockdecafe < quantitecafeMlatte) {
                    println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                  } else
                    println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

                } else if (choixdoselait == dose2lait) {
                  println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose2lait + " = " + " CHF " + (coutlatte + coutdose2lait))
                  if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocklait >= quantite2dose) {
                    stockdecafe -= quantitecafeMlatte
                    stocklait -= quantitelaitMlatte
                    stocklait -= quantite2dose
                    println("Veuillez payer en utilisant Twint.")

                    // Génération d'une chaîne alphanumérique de 5 caractères
                    val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                    val length = 5
                    val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                    // Affichage du code Twint
                    println(s"Votre code Twint : $codeTwint")
                    println("En attente de paiement...")
                    Thread.sleep(3000)
                    println("Merci ! Votre paiement a ete accepte.")
                    println("preparation de votre boisson ")
                    Thread.sleep(5000)
                    println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                  } else if (stockdecafe < quantitecafeMlatte) {
                    println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                  } else
                    println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

                } else
                  println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose3lait + " = " + " CHF " + (coutlattemoyen + coutdose3lait))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocklait >= quantite3dose) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocklait -= quantite3dose
                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } //sucre peu
            } else if (choixsucre == peudesucre) {
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")

              do {
                choixlait = readInt()
                if (choixlait != ouilait && choixlait != nonlait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
                }
              } while (choixlait != ouilait && choixlait != nonlait)
              if (choixlait == nonlait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n"
                  + "Niveau de sucre : Peu" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (3.30))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantitesucreexpresso) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocksucre -= quantitesucreexpresso

                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else if (stocklait < quantitelaitMlatte) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


              } else if (choixlait == ouilait)
                println("Combien de dose ?")
              do {
                choixdoselait = readInt()
                if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
                }

              } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
              if (choixdoselait == dose1lait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutlattemoyen + coutdose1lait + coutpeusucre))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantitesucreexpresso && stocklait >= quantite1dose) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocksucre -= quantitesucreexpresso
                  stocklait -= quantite1dose

                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else if (stocklait < quantitelaitMlatte + quantite1dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } else if (choixdoselait == dose2lait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (3.40))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantitesucreexpresso && stocklait >= quantite2dose) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocksucre -= quantitesucreexpresso
                  stocklait -= quantite2dose

                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else if (stocklait < quantitelaitMlatte + quantite2dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } else
                println("Boisson séleéctionnee : Latte (Moyen)" + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutlattemoyen + coutdose3lait + coutpeusucre))
              if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantitesucreexpresso && stocklait >= quantite3dose) {
                stockdecafe -= quantitecafeMlatte
                stocklait -= quantitelaitMlatte
                stocksucre -= quantitesucreexpresso
                stocklait -= quantite3dose

                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeMlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitMlatte + quantite3dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


              // sucre moyen

            } else if (choixsucre == moyendesucre) {
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


              do {
                choixlait = readInt()
                if (choixlait != ouilait && choixlait != nonlait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
                }
              } while (choixlait != ouilait && choixlait != nonlait)
              if (choixlait == nonlait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n"
                  + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.40))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteMsucreexpresso) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocksucre -= quantiteMsucreexpresso


                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else if (stocklait < quantitelaitMlatte) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


              } else if (choixlait == ouilait)
                println("Combien de dose ?")
              do {
                choixdoselait = readInt()
                if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
                }

              } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
              if (choixdoselait == dose1lait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (coutlattemoyen + coutdose1lait + coutmoyensucre))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteMsucreexpresso && stocklait >= quantite1dose) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocksucre -= quantiteMsucreexpresso
                  stocklait -= quantite1dose


                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else if (stocklait < quantitelaitMlatte + quantite1dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } else if (choixdoselait == dose2lait) {
                println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.50))
                if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteMsucreexpresso && stocklait >= quantite2dose) {
                  stockdecafe -= quantitecafeMlatte
                  stocklait -= quantitelaitMlatte
                  stocksucre -= quantiteMsucreexpresso
                  stocklait -= quantite2dose


                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeMlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else if (stocklait < quantitelaitMlatte + quantite2dose) {
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } else
                println("Boisson séleéctionnee : Latte (Moyen)" + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.55))
              if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteMsucreexpresso && stocklait >= quantite3dose) {
                stockdecafe -= quantitecafeMlatte
                stocklait -= quantitelaitMlatte
                stocksucre -= quantiteMsucreexpresso
                stocklait -= quantite3dose


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeMlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitMlatte + quantite3dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              // bcp de sucre
            } else
              println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Latte (Moyen) " + "\n"
                + "Niveau de sucre : Beaucoup" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattemoyen + coutbcpsucre))
              if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteBCPxpresso) {
                stockdecafe -= quantitecafeMlatte
                stocklait -= quantitelaitMlatte
                stocksucre -= quantiteBCPxpresso

                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeMlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitMlatte) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            } else if (choixlait == ouilait)
              println("Combien de dose ?")
            do {
              choixdoselait = readInt()
              if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
              }

            } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
            if (choixdoselait == dose1lait) {
              println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattemoyen + coutdose1lait + coutbcpsucre))
              if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteBCPxpresso && stocklait >= quantite1dose) {
                stockdecafe -= quantitecafeMlatte
                stocklait -= quantitelaitMlatte
                stocksucre -= quantiteBCPxpresso
                stocklait -= quantite1dose


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeMlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitMlatte + quantite1dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

            } else if (choixdoselait == dose2lait) {
              println("Boisson séleéctionnee : Latte (Moyen) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattemoyen + coutdose2lait + coutbcpsucre))
              if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteBCPxpresso && stocklait >= quantite2dose) {
                stockdecafe -= quantitecafeMlatte
                stocklait -= quantitelaitMlatte
                stocksucre -= quantiteBCPxpresso
                stocklait -= quantite2dose


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeMlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitMlatte + quantite2dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

            } else
              println("Boisson séleéctionnee : Latte (Moyen)" + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattemoyen + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattemoyen + coutdose3lait + coutbcpsucre))
            if (stockdecafe >= quantitecafeMlatte && stocklait >= quantitelaitMlatte && stocksucre >= quantiteBCPxpresso && stocklait >= quantite3dose) {
              stockdecafe -= quantitecafeMlatte
              stocklait -= quantitelaitMlatte
              stocksucre -= quantiteBCPxpresso
              stocklait -= quantite3dose


              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeMlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else if (stocklait < quantitelaitMlatte + quantite3dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


          } // GRAND LATTE

          else
            println("Souhaitez-vous ajouter du sucre ?" + "\n" + "1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
          do {
            choixsucre = readInt()
            if (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + " 1) Sans sucre" + "\n" + "2) Peu (5g)-CHF 0.10" + "\n" + "3) Moyen (10g)-CHF 0.20" + "\n" + "4) Beaucoup (15g)-CHF 0.30")
            }
          } while (choixsucre != peudesucre && choixsucre != moyendesucre && choixsucre != bcpdesucre && choixsucre != sanssucre)
          if (choixsucre == sanssucre) {
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")
            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Latte (Grand) " + "\n"
                + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire : Non " + "\n" + coutlattegrand + "CHF")
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            } else if (choixlait == ouilait) {
              println("Combien de dose ?")
              do {
                choixdoselait = readInt()
                if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                  println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3")
                }
              } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
              if (choixdoselait == dose1lait) {
                println("Boisson séleéctionnee : Latte (Grand)" + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose1lait + " = " + " CHF " + (3.8))
                if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite1dose) {
                  stockdecafe -= quantitecafeGlatte
                  stocklait -= quantitelaitGlatte
                  stocklait -= quantite1dose


                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeGlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } else if (choixdoselait == dose2lait) {
                println("Boisson séleéctionnee : Latte (Grand) " + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose2lait + " = " + " CHF " + (3.85))
                if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite2dose) {
                  stockdecafe -= quantitecafeGlatte
                  stocklait -= quantitelaitGlatte
                  stocklait -= quantite2dose


                  println("Veuillez payer en utilisant Twint.")

                  // Génération d'une chaîne alphanumérique de 5 caractères
                  val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                  val length = 5
                  val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                  // Affichage du code Twint
                  println(s"Votre code Twint : $codeTwint")
                  println("En attente de paiement...")
                  Thread.sleep(3000)
                  println("Merci ! Votre paiement a ete accepte.")
                  println("preparation de votre boisson ")
                  Thread.sleep(5000)
                  println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

                } else if (stockdecafe < quantitecafeGlatte) {
                  println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
                } else
                  println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              } else
                println("Boisson séleéctionnee : Latte (Grand)" + "\n" + "Niveau de sucre : Sans sucre " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose3lait + " = " + " CHF " + (3.90))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite3dose) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocklait -= quantite3dose


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

            } //sucre peu
          } else if (choixsucre == peudesucre) {
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")

            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Latte (Grand) " + "\n"
                + "Niveau de sucre : Peu" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (3.80))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocksucre >= quantitesucreexpresso) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocksucre -= quantitesucreexpresso

                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitGlatte) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            } else if (choixlait == ouilait)
              println("Combien de dose ?")
            do {
              choixdoselait = readInt()
              if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
              }

            } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
            if (choixdoselait == dose1lait) {
              println("Boisson séleéctionnee : Latte (Grand) " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutlattegrand + coutdose1lait + coutpeusucre))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite1dose && stocksucre >= quantitesucreexpresso) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocklait -= quantite1dose
                stocksucre -= quantitesucreexpresso


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitGlatte + quantite1dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

            } else if (choixdoselait == dose2lait) {
              println("Boisson séleéctionnee : Latte (Grand) " + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (3.90))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite2dose && stocksucre >= quantitesucreexpresso) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocklait -= quantite2dose
                stocksucre -= quantitesucreexpresso


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitGlatte + quantite2dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            } else
              println("Boisson séleéctionnee : Latte (Grand)" + "\n" + "Niveau de sucre : Peu " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutpeusucre + " = " + " CHF " + (coutlattegrand + coutdose3lait + coutpeusucre))
            if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite3dose && stocksucre >= quantitesucreexpresso) {
              stockdecafe -= quantitecafeGlatte
              stocklait -= quantitelaitGlatte
              stocklait -= quantite3dose
              stocksucre -= quantitesucreexpresso


              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeGlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else if (stocklait < quantitelaitGlatte + quantite3dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            // sucre moyen

          } else if (choixsucre == moyendesucre) {
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


            do {
              choixlait = readInt()
              if (choixlait != ouilait && choixlait != nonlait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
              }
            } while (choixlait != ouilait && choixlait != nonlait)
            if (choixlait == nonlait) {
              println("Boisson séleéctionnee : Latte (Moyen) " + "\n"
                + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.90))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocksucre >= quantiteMsucreexpresso) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocksucre -= quantiteMsucreexpresso

                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitGlatte) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

              //PAIEMENT ECT


            } else if (choixlait == ouilait)
              println("Combien de dose ?")
            do {
              choixdoselait = readInt()
              if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
                println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
              }

            } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
            if (choixdoselait == dose1lait) {
              println("Boisson séleéctionnee : Latte (Grand) " + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (3.95))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite1dose && stocksucre >= quantiteMsucreexpresso) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocklait -= quantite1dose
                stocksucre -= quantiteMsucreexpresso


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitGlatte + quantite1dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

            } else if (choixdoselait == dose2lait) {
              println("Boisson séleéctionnee : Latte (Grand)" + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (4.0))
              if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite2dose && stocksucre >= quantiteMsucreexpresso) {
                stockdecafe -= quantitecafeGlatte
                stocklait -= quantitelaitGlatte
                stocklait -= quantite2dose
                stocksucre -= quantiteMsucreexpresso


                println("Veuillez payer en utilisant Twint.")

                // Génération d'une chaîne alphanumérique de 5 caractères
                val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                val length = 5
                val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

                // Affichage du code Twint
                println(s"Votre code Twint : $codeTwint")
                println("En attente de paiement...")
                Thread.sleep(3000)
                println("Merci ! Votre paiement a ete accepte.")
                println("preparation de votre boisson ")
                Thread.sleep(5000)
                println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

              } else if (stockdecafe < quantitecafeGlatte) {
                println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else if (stocklait < quantitelaitGlatte + quantite2dose) {
                println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
              } else
                println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            } else
              println("Boisson séleéctionnee : Latte (Grand)" + "\n" + "Niveau de sucre : Moyen " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutmoyensucre + " = " + " CHF " + (4.05))
            if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite3dose && stocksucre >= quantiteMsucreexpresso) {
              stockdecafe -= quantitecafeGlatte
              stocklait -= quantitelaitGlatte
              stocklait -= quantite3dose
              stocksucre -= quantiteMsucreexpresso


              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeGlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else if (stocklait < quantitelaitGlatte + quantite3dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

            // bcp de sucre
          } else
            println("Souhaitez-vous ajouter du lait en supplement ?)" + "\n" + "(Disponible uniquement pour Cappuccino et Latte)" + "\n" + "1) OUI" + "\n" + "2) NON")


          do {
            choixlait = readInt()
            if (choixlait != ouilait && choixlait != nonlait) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1) OUI" + "\n" + "2) NON" + "\n")
            }
          } while (choixlait != ouilait && choixlait != nonlait)

          if (choixlait == nonlait) {
            println("Boisson séleéctionnee : Latte (Grand) " + "\n"
              + "Niveau de sucre : Beaucoup" + "\n" + "Lait suplementaire : Non " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattegrand + coutbcpsucre))
            if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocksucre >= quantiteBCPxpresso) {
              stockdecafe -= quantitecafeGlatte
              stocklait -= quantitelaitGlatte
              stocksucre -= quantiteBCPxpresso

              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeGlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else if (stocklait < quantitelaitGlatte) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


            //PAIEMENT ECT


          } else if (choixlait == ouilait)
            println("Combien de dose ?")
          do {
            choixdoselait = readInt()
            if (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait) {
              println("Option non valide" + "\n" + "Veuillez choisir entre :" + "\n" + "1)" + "\n" + "2)" + "\n" + "3)")
            }

          } while (choixdoselait != dose1lait && choixdoselait != dose2lait && choixdoselait != dose3lait)
          if (choixdoselait == dose1lait) {
            println("Boisson séleéctionnee : Latte (Grand) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  1 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose1lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattegrand + coutdose1lait + coutbcpsucre))
            if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite1dose && stocksucre >= quantiteBCPxpresso) {
              stockdecafe -= quantitecafeGlatte
              stocklait -= quantitelaitGlatte
              stocklait -= quantite1dose
              stocksucre -= quantiteBCPxpresso


              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeGlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else if (stocklait < quantitelaitGlatte + quantite1dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


          } else if (choixdoselait == dose2lait) {
            println("Boisson séleéctionnee : Latte (Grand) " + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  2 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose2lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (4.10))
            if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite2dose && stocksucre >= quantiteBCPxpresso) {
              stockdecafe -= quantitecafeGlatte
              stocklait -= quantitelaitGlatte
              stocklait -= quantite2dose
              stocksucre -= quantiteBCPxpresso


              println("Veuillez payer en utilisant Twint.")

              // Génération d'une chaîne alphanumérique de 5 caractères
              val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
              val length = 5
              val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

              // Affichage du code Twint
              println(s"Votre code Twint : $codeTwint")
              println("En attente de paiement...")
              Thread.sleep(3000)
              println("Merci ! Votre paiement a ete accepte.")
              println("preparation de votre boisson ")
              Thread.sleep(5000)
              println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

            } else if (stockdecafe < quantitecafeGlatte) {
              println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else if (stocklait < quantitelaitGlatte + quantite2dose) {
              println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
            } else
              println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")

          } else
            println("Boisson séleéctionnee : Latte (Grand)" + "\n" + "Niveau de sucre : Beaucoup " + "\n" + "Lait suplementaire :  3 dose " + "\n" + "Prix total: " + " CHF " + coutlattegrand + " + " + " CHF " + coutdose3lait + " + " + " CHF " + coutbcpsucre + " = " + " CHF " + (coutlattegrand + coutdose3lait + coutbcpsucre))
          if (stockdecafe >= quantitecafeGlatte && stocklait >= quantitelaitGlatte && stocklait >= quantite3dose && stocksucre >= quantiteBCPxpresso) {
            stockdecafe -= quantitecafeGlatte
            stocklait -= quantitelaitGlatte
            stocklait -= quantite3dose
            stocksucre -= quantiteBCPxpresso


            println("Veuillez payer en utilisant Twint.")

            // Génération d'une chaîne alphanumérique de 5 caractères
            val alphanumericChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
            val length = 5
            val codeTwint = (1 to length).map(_ => alphanumericChars(Random.nextInt(alphanumericChars.length))).mkString

            // Affichage du code Twint
            println(s"Votre code Twint : $codeTwint")
            println("En attente de paiement...")
            Thread.sleep(3000)
            println("Merci ! Votre paiement a ete accepte.")
            println("preparation de votre boisson ")
            Thread.sleep(5000)
            println("Votre Latte est pret  ! " + " Bonne dégustation ! ")

          } else if (stockdecafe < quantitecafeGlatte) {
            println("Erreur : Quantité de cafe insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
          } else if (stocklait < quantitelaitGlatte + quantite3dose) {
            println("Erreur : Quantité de lait insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")
          } else
            println("Erreur : Quantité de sucre insuffisante pour preparer la boisson sélectionnée. Veuillez choisir une taille plus petite ou essayer une autre boisson")


        }
      } else if (mode == admin) {
        var stockactuelcafe = 0
        var stockactuelsucre = 0
        var stockactuellait = 0
        val codepardefaut = 434343
        println("Mode Admin")

        println("saisir le code PIN :  ******")
        var codePin = 0
        var valide = false
        while (!valide) {
          codePin = readInt()
          if (codePin == codepardefaut) {
            valide = true
            println("Accès autorisé .")
            println("Stocks : ")
            println("Poudre de café : " + stockdecafe + "g")
            stockactuelcafe = stockdecafe
            println("Sucre : " + stocksucre + "g")
            stockactuelsucre = stocksucre
            println("Lait : " + stocklait + "ml")
            stockactuellait = stocklait

            println("Reapprovisionnement  des stocks ... \n  Ajout :")

            var caffeajout = 0
            var valide1 = false
            while (!valide1) {
              println("Poudre de cafee : ")
              caffeajout = readInt()
              if (caffeajout >= 0) {
                valide1 = true
              } else {
                println("erreur la valeur saissie doit etre positive ")
              }
            }
            stockdecafe = stockactuelcafe + caffeajout
            var Laitajout = 0
            var valide2 = false
            while (!valide2) {
              println("Lait : ")
              Laitajout = readInt()
              if (Laitajout >= 0) {
                valide2 = true
              } else {
                println("erreur la valeur saissie doit etre positive ")
              }
            }
            stocklait = stockactuellait + Laitajout

            var sucreajout = 0
            var valide3 = false
            while (!valide3) {
              println("sucre : ")
              sucreajout = readInt()
              if (Laitajout >= 0) {
                valide3 = true
              } else {
                println("erreur la valeur saissie doit etre positive ")
              }
            }
            stocksucre = stockactuelsucre + sucreajout

          } else {
            println("Le code PIN est invalide ")
          }
        }

      }

      else {
        println("Merci de votre visite " + "Bonne journée")
      }

    } while (continuer)





  }



}













































