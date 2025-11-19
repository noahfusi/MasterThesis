import javax.swing.plaf.OptionPaneUI
import scala.io.StdIn.readLine
import scala.language.postfixOps
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {

    var uni = 0
    var Sucre = 30
    var Café = 50
    var Lait = 500
    val nbMachines = 5


    while (uni == 0) {
      println("Bienvenue chez Nospresso - Sélectionnez une machine :")
      for (i <- 0 until nbMachines) println(s"Machine $i")
      val machine = readLine(">") toInt

      if (machine == 1)
      println(" Nospresso Caf´e")
      println("Veuillez selectionner votre mode")
      println("1) Client")
      println("2) Admin")
      println("3) Quitter")
      val choice = readLine(">") toInt

      if (choice > 3 || choice < 1) {
        println("Erreur, veuillez saisir une valeur correcte.")
        Thread.sleep(5000)
        println("" +
          "" +
          "")
        uni = 0
      }

      if (choice == 1) {
        println("Veuillez sélectionner votre boisson :")
        println(" 1) Expresso - CHF 2.00")
        println(" 2) Cappucinno - CHF 2.50")
        println(" 3) Latte - CHF 2.70 (Petit), 4) CHF 3.20 (Moyen), 5) CHF 3.70 (Grand)")
        val choic = readLine(">").toInt

        if (choic == 1) {
          Café = Café - 8
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          val choi = readLine(">") toInt


          if (choi == 1) {
            println("Boisson sélectionnée : Expresso")
            println("Niveau de sucre : Sans sucre")
            println("Prix total : CHF 2.00")
          }

          if (choi == 2) {
            Sucre = Sucre - 5
            println("Boisson sélectionnée : Expresso ")
            println("Niveau de sucre : Peu (5g)")
            println("Prix total : CHF 2.00 + CHF 0.10 = CHF 2.10")
          }

          if (choi == 3) {
            Sucre = Sucre - 10
            println("Boisson sélectionnée : Expresso ")
            println("Niveau de sucre : Moyen (10g)")
            println("Prix total : CHF 2.00 + CHF 0.20 = CHF 2.20")
          }

          if (choi == 4) {
            Sucre = Sucre - 15
            println("Boisson sélectionnée : Expresso ")
            println("Niveau de sucre : (15g)")
            println("Prix total : CHF 2.00 + CHF 0.30 = CHF 2.30")
          }
          Thread.sleep(5000)


        }

        if (choic == 2) {
          Café = Café - 6
          Lait = Lait - 100
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          val choi = readLine(">") toInt

          if (choi == 1) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Lait = Lait - 50
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.50 + CHF 0.05 = CHF 2.55")
              }
              if (ch == 2) {
                Lait = Lait - 100
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.50 + CHF 0.10 = CHF 2.60")
              }
              if (ch == 3) {
                Lait = Lait - 150
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.50 + CHF 0.15 = CHF 2.65")
              }

            }
            if (cho == 2) {
              println("Boisson sélectionnée : Cappuccino ")
              println("Niveau de sucre : Sans sucre ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.50")
            }

          }
          if (choi == 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 5
                Lait = Lait - 50
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.50 + CHF 0.10 + CHF 0.05 = CHF 2.65")
              }
              if (ch == 2) {
                Sucre = Sucre - 5
                Lait = Lait - 100
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.50 + CHF 0.10 + CHF 0.10 = CHF 2.70")
              }
              if (ch == 3) {
                Sucre = Sucre - 5
                Lait = Lait - 150
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.50 + CHF 0-10 + CHF 0.15 = CHF 2.75")
              }

            }
            if (cho == 2) {
              Sucre = Sucre - 5
              println("Boisson sélectionnée : Cappuccino ")
              println("Niveau de sucre : Peu (5g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.50 + CHF 0.10 = CHF 2.60")
            }

          }
          if (choi == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 10
                Lait = Lait - 50
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.50 + CHF 0.20 + CHF 0.05 = CHF 2.75")
              }
              if (ch == 2) {
                Sucre = Sucre - 10
                Lait = Lait - 100
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.50 + CHF 0.20 + CHF 0.10 = CHF 2.80")
              }
              if (ch == 3) {
                Sucre = Sucre - 10
                Lait = Lait - 150
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.50 + CHF 0.20 + CHF 0.15 = CHF 2.85")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 10
              println("Boisson sélectionnée : Cappuccino ")
              println("Niveau de sucre : Moyen (10g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.50 + CHF 0.20 = CHF 2.70")
            }
          }
          if (choi == 4) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 15
                Lait = Lait - 50
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.50 + CHF 0.30 + CHF 0.05 = CHF 2.85")
              }
              if (ch == 2) {
                Sucre = Sucre - 15
                Lait = Lait - 100
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.50 + CHF 0.30 + CHF 0.10 = CHF 2.90")
              }
              if (ch == 3) {
                Sucre = Sucre - 15
                Lait = Lait - 150
                println("Boisson sélectionnée : Cappuccino ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.50 + CHF 0.30 + CHF 0.15 = CHF 2.95")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 15
              println("Boisson sélectionnée : Cappuccino ")
              println("Niveau de sucre : Beaucoup (15g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.50 + CHF 0.30 = CHF 2.80")
            }
          }
          Thread.sleep(5000)
        }

        if (choic == 3) {
          Café = Café - 6
          Lait = Lait - 120
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          val choi = readLine(">").toInt

          if (choi == 1) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.70 + CHF 0.05 = CHF 2.75")
              }
              if (ch == 2) {
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.70 + CHF 0.10 = CHF 2.80")
              }
              if (ch == 3) {
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.70 + CHF 0.15 = CHF 2.85")
              }
            }

            if (cho == 2) {
              println("Boisson sélectionnée : Latte (Petit) ")
              println("Niveau de sucre : Sans sucre ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.70")
            }

          }
          if (choi == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 10
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.70 + CHF 0.20 + CHF 0.05 = CHF 2.95")
              }
              if (ch == 2) {
                Sucre = Sucre - 10
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.70 + CHF 0.20 + CHF 0.10 = CHF 3.00")
              }
              if (ch == 3) {
                Sucre = Sucre - 10
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.70 + CHF 0.20 + CHF 0.15 = CHF 3.05")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 10
              println("Boisson sélectionnée : Latte (Petit) ")
              println("Niveau de sucre : Moyen (10g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.70 + CHF 0.20 = CHF 2.90")
            }

          }
          if (choi == 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 5
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.70 + CHF 0.10 + CHF 0.05 = CHF 2.85")
              }
              if (ch == 2) {
                Sucre = Sucre - 5
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.70 + CHF 0.10 + CHF 0.10 = CHF 2.90")
              }
              if (ch == 3) {
                Sucre = Sucre - 5
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.70 + CHF 0.10 + CHF 0.15 = CHF 2.95")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 5
              println("Boisson sélectionnée : Latte (Petit) ")
              println("Niveau de sucre : Peu (%g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.70 + CHF 0.10 = CHF 2.80")
            }

          }
          if (choi == 4) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 15
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 2.70 + CHF 0.30 + CHF 0.05 = CHF 3.05")
              }
              if (ch == 2) {
                Sucre = Sucre - 15
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 2.70 + CHF 0.30 + CHF 0.10 = CHF 3.10")
              }
              if (ch == 3) {
                Sucre = Sucre - 15
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Petit) ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 2.70 + CHF 0.30 + CHF 0.15 = CHF 3.15")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 15
              println("Boisson sélectionnée : Latte (Petit) ")
              println("Niveau de sucre : Beaucoup (15g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 2.70 + CHF 0.30 = CHF 3.00")
            }

          }
        }


        if (choic == 4) {
          Café = Café - 8
          Lait = Lait - 150
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          val choi = readLine(">").toInt

          if (choi == 1) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.20 + CHF 0.05 = CHF 3.25")
              }
              if (ch == 2) {
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.20 + CHF 0.10 = CHF 3.30")
              }
              if (ch == 3) {
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.20 + CHF 0.15 = CHF 3.35")
              }
            }

            if (cho == 2) {
              println("Boisson sélectionnée : Latte (Moyen) ")
              println("Niveau de sucre : Sans sucre ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.20")
            }

          }
          if (choi == 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 5
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.20 + CHF 0.10 + CHF 0.05 = CHF 3.35")
              }
              if (ch == 2) {
                Sucre = Sucre - 5
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.20 + CHF 0.10 + CHF 0.10 = CHF 3.40")
              }
              if (ch == 3) {
                Sucre = Sucre - 5
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.20 + CHF 0.10 + CHF 0.15 = CHF 3.45")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 5
              println("Boisson sélectionnée : Latte (Moyen) ")
              println("Niveau de sucre : Peu (5g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.20 + CHF 0.10 = CHF 3.30")
            }

          }
          if (choi == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 10
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.20 + CHF 0.20 + CHF 0.05 = CHF 3.45")
              }
              if (ch == 2) {
                Sucre = Sucre - 10
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.20 + CHF 0.20 + CHF 0.10 = CHF 3.50")
              }
              if (ch == 3) {
                Sucre = Sucre - 10
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.20 + CHF 0.20 + CHF 0.15 = CHF 3.55")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 10
              println("Boisson sélectionnée : Latte (Moyen) ")
              println("Niveau de sucre : Moyen (10g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.20 + CHF 0.20 = CHF 3.40")
            }

          }
          if (choi == 4) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 15
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.20 + CHF 0.30 + CHF 0.05 = CHF 3.55")
              }
              if (ch == 2) {
                Sucre = Sucre - 15
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.20 + CHF 0.30 + CHF 0.10 = CHF 3.60")
              }
              if (ch == 3) {
                Sucre = Sucre - 15
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Moyen) ")
                println("Niveau de sucre : Beaucoup (15g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.20 + CHF 0.30 + CHF 0.15 = CHF 3.65")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 15
              println("Boisson sélectionnée : Latte (Moyen) ")
              println("Niveau de sucre : Beaucoup (15g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.20 + CHF 0.30 = CHF 3.50")
            }

          }
        }

        if (choic == 5) {
          Café = Café - 12
          Lait = Lait - 200
          println("Souhaitez-vous ajouter du sucre ?")
          println("1) Sans sucre")
          println("2) Peu (5g) - CHF 0.10")
          println("3) Moyen (10g) - CHF 0.20")
          println("4) Beaucoup (15g) - CHF 0.30")
          val choi = readLine(">").toInt

          if (choi == 1) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.70 + CHF 0.05 = CHF 3.75")
              }
              if (ch == 2) {
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.70 + CHF 0.10 = CHF 3.80")
              }
              if (ch == 3) {
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Sans sucre ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.70 + CHF 0.15 = CHF 3.85")
              }
            }

            if (cho == 2) {
              println("Boisson sélectionnée : Latte (Grand) ")
              println("Niveau de sucre : Sans sucre ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.70")
            }

          }
          if (choi == 2) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 5
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.70 + CHF 0.10 + CHF 0.05 = CHF 3.85")
              }
              if (ch == 2) {
                Sucre = Sucre - 5
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.70 + CHF 0.10 + CHF 0.10 = CHF 3.90")
              }
              if (ch == 3) {
                Sucre = Sucre - 5
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Peu (5g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.70 + CHF 0.10 + CHF 0.15 = CHF 3.95")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 5
              println("Boisson sélectionnée : Latte (Grand) ")
              println("Niveau de sucre : Peu (5g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.70 + CHF 0.10 = CHF 3.80")
            }

          }
          if (choi == 3) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 10
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.70 + CHF 0.20 + CHF 0.05 = CHF 3.95")
              }
              if (ch == 2) {
                Sucre = Sucre - 10
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.70 + CHF 0.20 + CHF 0.10 = CHF 4.00")
              }
              if (ch == 3) {
                Sucre = Sucre - 10
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Moyen (10g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.70 + CHF 0.20 + CHF 0.15 = CHF 4.05")
              }
            }

            if (cho == 2) {
              Sucre = Sucre - 10
              println("Boisson sélectionnée : Latte (Grand) ")
              println("Niveau de sucre : Moyen (10g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.70 + CHF 0.20 = CHF 3.90")
            }

          }
          if (choi == 4) {
            println("Souhaitez-vous ajouter du lait en supplément ?")
            println("1) Oui")
            println("2) Non")
            val cho = readLine(">") toInt

            if (cho == 1) {
              println("Combien de dose ?")
              val ch = readLine(">") toInt

              if (ch == 1) {
                Sucre = Sucre - 15
                Lait = Lait - 50
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Grand (15g) ")
                println("Lait en supplément : Une dose ")
                println("Prix total : CHF 3.70 + CHF 0.30 + CHF 0.05 = CHF 4.05")
              }
              if (ch == 2) {
                Sucre = Sucre - 15
                Lait = Lait - 100
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Grand (15g) ")
                println("Lait en supplément : Deux dose ")
                println("Prix total : CHF 3.70 + CHF 0.30 + CHF 0.10 = CHF 4.10")
              }
              if (ch == 3) {
                Sucre = Sucre - 15
                Lait = Lait - 150
                println("Boisson sélectionnée : Latte (Grand) ")
                println("Niveau de sucre : Grand (15g) ")
                println("Lait en supplément : Trois dose ")
                println("Prix total : CHF 3.70 + CHF 0.30 + CHF 0.15 = CHF 4.15")
              }
            }
            if (cho == 2) {
              Sucre = Sucre - 15
              println("Boisson sélectionnée : Latte (Grand) ")
              println("Niveau de sucre : Grand (15g) ")
              println("Lait en supplément : Non ")
              println("Prix total : CHF 3.70 + CHF 0.30 = CHF 4.00")
            }

          }
        }
        Thread.sleep(5000)
        if (Café <= 0) {
          println("Erreur : Quantité de Café insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
        if (Sucre <= 0) {
          println("Erreur : Quantité de Sucre insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }
        if (Lait <= 0) {
          println("Erreur : Quantité de Lait insuffisante pour préparer la boisson sélectionnée.")
          println("Veuillez choisir une taille plus petite ou essayer une autre boisson.")
        }

        else {
          println("" +
            "")
          println("Veuillez payer en utilisant Twint.")
          val twint: String = (1 to 5).map(_ => Random.alphanumeric.head).mkString
          println("Votre code de paiement est :" + twint)
          println("(En attente de validation du paiement...)")
          Thread.sleep(3000)
          println("" +
            "")
          println("Paiement confirmé.")
          println("Préparation de votre boisson...")

          if (choic == 1) {
            println("Votre Expresso est prêt ! Bonne dégustation !")
          }
          if (choic == 2) {
            println("Votre Cappuccino est prêt ! Bonne dégustation !")
          }
          if (choic == 3) {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
          if (choic == 4) {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
          if (choic == 5) {
            println("Votre Latte est prêt ! Bonne dégustation !")
          }
          println("" +
            "" +
            "")
          Thread.sleep(5000)
          uni = 0

        }
      }
      if (choice == 2) {
        println("Mode admin")
        println("Entrez le code PIN :")
        var PIN = readLine(">") toInt
        var tentative = 3


        if (PIN < 434343 || PIN > 434343){
          tentative = tentative - 1
        println("Accès refusé " + tentative + " tentatives restantes.")
          var PIN = readLine(">") toInt

          if (PIN < 434343 || PIN > 434343){
            tentative = tentative - 1
            println("Accès refusé " + tentative + " tentative restante.")
            var PIN = readLine(">") toInt

            if (PIN < 434343 || PIN > 434343) {
              tentative = tentative - 1
              println("Accès refusé " + tentative + " tentative restante.")
            }}}

          if (tentative == 0){
            println("Fin du programme.")
            uni = 1
          }

          else {
            var code = readLine("Voulez vous redéfinir le code PIN ?    1) Oui.   2)Non. >") toInt

            while (code<1 || code>2) {
                println("" +
                  "Veuillez saisir une valeur correcte.")
                println("Voulez vous redéfinir le code PIN ?    1) Oui.   2)Non.")
                code = readLine(">") toInt
              }

            if(code == 1) {
            var newPin = readLine("Entrez votre nouveau code PIN à 6 chiffres. > ")
            while (newPin.length != 6) {
              newPin = readLine("Entrez votre nouveau code PIN à 6 chiffres. > ") }}

              if(code == 2 || code == 1) {
              println("Accès autorisé.")
              println("" +
            "" +
            "")
          println("Stocks:")
          println("" +
            "")
          println("Café : " + Café)
          println("Lait : " + Lait)
          println("Sucre : " + Sucre)
          println("" +
            "")
          println("Réapprovisionnement des stocks...")
          println("Ajout :")
          val ajout = readLine("Café supplémentaire (g) >") toInt
          val ajou = readLine("Lait supplémentaire (mL) >") toInt
          val ajo = readLine("Sucre supplémentaire (g) >").toInt
          val résultat = Café + ajout
          val résulta = Lait + ajou
          val résult = Sucre + ajo
          println("" +
            "")
          println("Café : "+ résultat )
          println("Lait : "+ résulta)
          println("Sucre : "+ résult)
          Thread.sleep(2000)
          println("" +
            "" )
          println("Niveau de stock mis à jour.")
          println("Retour au menu principal...")
          println("" +
            "" +
            "")
          Thread.sleep(5000)
          uni = 0 }

      }
      if (choice == 3) {
        println("Merci, au revoir.")
        println("" +
          "" +
          "Fin du programme.")
        uni = 1
      }
    }}}}