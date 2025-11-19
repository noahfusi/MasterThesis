import scala.io.StdIn.{readDouble, readInt}

object Main {
    def main(args: Array[String]): Unit = {
        var Boisson$ = ""
        var Sucre$ = ""
        var Lait$ = ""
        var PrixTotal$ = ""
        var cost = 0.0
        var lcost = 0.0
        var qteCafe = 0
        var qteSucre = 0
        var qteLait = 0.0
        var stockCafe = 50
        var stockSucre = 30
        var stockLait = 0.500
        var erreur = false
        var choix_initial = 0
        var choix_boisson = 0
        var choix = 0
        var choix_lait = 0
        val car_mdp = Array('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
        val mdp = Array.fill(5)('0')
        while (choix_initial != 3) {
            print("        Nospresso Café\nVeuillez sélectionner votre mode :\n1) Client\n2) Admin\n3) Quitter\n>")
            choix_initial = readInt()
            if (choix_initial == 1) {
                erreur = true
                while (erreur) {
                    choix_boisson = 0
                    qteCafe = 0
                    qteSucre = 0
                    qteLait = 0
                    cost = 0.0
                    while ((choix_boisson < 1) || (choix_boisson > 3)) {
                        print("\nVeuillez sélectionner votre boisson :\n1) Expresso - CHF 2.00\n2) Capuccino - CHF 2.50\n3) Latte - CHF 2.70 (Petit), CHF 3.20 (Moyen), CHF 3.70 (Grand)\n>")
                        choix_boisson = readInt()
                        if (choix_boisson == 1) {
                            Boisson$ = "Expresso"
                            PrixTotal$ = "CHF 2.00"
                            cost = 2
                            qteCafe = 8
                            qteLait = 0
                            Lait$ = "Non"
                        } else if (choix_boisson == 2) {
                            Boisson$ = "Capuccino"
                            PrixTotal$ = "CHF 2.50"
                            cost = 2.5
                            qteCafe = 6
                            qteLait = 0.10
                        } else if (choix_boisson == 3) {
                            choix = 0
                            while ((choix < 1) || (choix > 3)) {
                                print("\nQuelle taille de Latte désirez-vous ?\n1) Petit - CHF 2.70\n2) Moyen - CHF 3.20\n3) Grand - CHR 3.70\n>")
                                choix = readInt()
                                if (choix == 1) {
                                    Boisson$ = "Latte (Petit)"
                                    PrixTotal$ = "CHF 2.70"
                                    cost = 2.7
                                    qteCafe = 6
                                    qteLait = 0.120
                                } else if (choix == 2) {
                                    Boisson$ = "Latte (Moyen)"
                                    PrixTotal$ = "CHF 3.20"
                                    cost = 3.2
                                    qteCafe = 8
                                    qteLait = 0.150
                                } else if (choix == 3) {
                                    Boisson$ = "Latte (Grand)"
                                    PrixTotal$ = "CHF 3.70"
                                    cost = 3.7
                                    qteCafe = 12
                                    qteLait = 0.200
                                }
                            }
                        }
                        choix = 0
                        while ((choix < 1) || (choix > 4)) {
                            print("\nSouhaitez-vous ajouter du sucre ?\n1) Sans sucre\n2) Peu (5g) - CHF 0.10\n3) Moyen (10g) - CHF 0.20\n4) Beaucoup (15g) - CHF 0.30\n>")
                            choix = readInt()
                            if (choix == 1) {
                                Sucre$ = "Sans sucre"
                                qteSucre = 0
                            } else if (choix == 2) {
                                Sucre$ = "Peu (5g)"
                                qteSucre = 5
                                cost += 0.1
                                PrixTotal$ = PrixTotal$ + " + CHF 0.10"
                            } else if (choix == 3) {
                                Sucre$ = "Moyen (10g)"
                                qteSucre = 10
                                cost += 0.2
                                PrixTotal$ = PrixTotal$ + " + CHF 0.20"
                            } else if (choix == 4) {
                                Sucre$ = "Beaucoup (15 g)"
                                qteSucre = 15
                                cost += 0.3
                                PrixTotal$ = PrixTotal$ + " + CHF 0.30"
                            }
                        }
                        if (choix_boisson != 1) {
                            choix = 0
                            while ((choix < 1) || (choix > 2)) {
                                print("\nSouhaitez-vous ajouter du lait en supplément ?\n(Disponible uniquement pour Capuccino et Latte)\n1) Oui\n2) Non\n>")
                                choix = readInt()
                                if (choix == 1) {
                                    choix_lait = 0
                                    while ((choix_lait < 1) || (choix_lait > 3)) {
                                        print("\nCombien de doses (1-3) ?\n>")
                                        choix_lait = readInt()
                                        if ((choix_lait > 0) && (choix_lait < 4)) {
                                            Lait$ = "Oui, " + choix_lait + " dose"
                                            if (choix_lait > 1) {
                                                Lait$ = Lait$ + "s"
                                            }
                                            qteLait += choix_lait * 0.050
                                            lcost = 0.05 * choix_lait
                                            PrixTotal$ = PrixTotal$ + " + CHF " + f"$lcost%1.2f"
                                            cost += lcost
                                        }
                                    }
                                } else if (choix == 2) {
                                    Lait$ = "Non"
                                }
                            }
                        }
                    }
                    println("\nBoisson sélectionnée : " + Boisson$)
                    println("Niveau de sucre : " + Sucre$)
                    println("Lait supplémentaire : " + Lait$)
                    if (qteCafe > stockCafe) {
                        println("\nErreur : Quantité de poudre de café insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les\nstocks en mode Admin.")
                    } else if (qteLait > stockLait) {
                        if (choix_boisson == 2) {
                            println("\nErreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une autre boisson ou vérifier les \nstocks en mode Admin.")
                        } else {
                            println("\nErreur : Quantité de lait insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez choisir une taille plus petite ou essayer\nune autre boisson.")
                        }
                    } else if (qteSucre > stockSucre) {
                        println("\nErreur : Quantité de sucre insuffisante pour\npréparer la boisson sélectionnée.\nVeuillez modifier la dose de sucre ou choisir\nune autre boisson.")
                    } else {
                        erreur = false
                    }
                }
                stockCafe -= qteCafe
                stockLait -= qteLait
                stockSucre -= qteSucre
                if (PrixTotal$.length < 12) {
                    println("Prix total : " + PrixTotal$)
                } else {
                    println("Prix total : " + PrixTotal$ + " = CHF " + f"$cost%1.2f")
                }
                for (i <- 0 to 4) {
                    mdp(i) = car_mdp((car_mdp.length * math.random()).toInt)
                }
                println("\nVeuillez payer en utilisant Twint.\nVotre code de paiement est " + mdp.mkString + "\n(En attente de validation de paiement...)")
                Thread.sleep(3000)
                println("\nMerci ! Votre paiement a été accepté.\nPréparation de votre boisson...")
                Thread.sleep(5000)
                println("\nVotre " + Boisson$ + " est prêt ! Bonne dégustation !")

            } else if (choix_initial == 2) {
                print("\nMode Admin\nEntrez le code PIN : ")
                choix = readInt()
                if (choix == 434343) {
                    println("Accès autorisé.\n\nStocks :\n")
                    println("    Poudre de café: " + stockCafe + "g\n    Lait          : " + f"$stockLait%1.2f" + "L\n    Sucre         : " + stockSucre + "g\n\nRéapprovisionnement des stocks...\nAjout :")
                    choix = -1
                    while ((choix < 0) || (choix > 50)) {
                        print("    Poudre de café: ")
                        choix = readInt()
                    }
                    stockCafe += choix
                    lcost = -1.0
                    while ((lcost < 0.0) || (lcost > 0.5)) {
                        print("    Lait          : ")

                        lcost = readDouble()
                    }
                    stockLait += lcost
                    choix = -1
                    while ((choix < 0) || (choix > 30)) {
                        print("    Sucre         : ")
                        choix = readInt()
                    }
                    stockSucre += choix
                    println("Niveaux de stock mis à jour.\nRetour au menu principal...\n")
                } else {
                    println("Code erronné. Accès refusé.\n")
                }
            }
            println
        }
    }
}
