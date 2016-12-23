PROGRAM factorielle

! Compute the factorial of a number.
* If the input number is negative, print -1.

  INTEGER nombre, resultat, compteur, compteur2
  READ*, nombre
  resultat = 1

  IF (nombre .GE. 0 .AND. nombre .LE. 100) THEN
    DO compteur = 1, 5
      DO compteur2 = 1, 2
        resultat = resultat * compteur
        PRINT*, resultat
      ENDDO
    ENDDO
    PRINT*, 10/2
  ENDIF
  PRINT*, (1*2)
  IF (nombre .GT. 100 .AND. nombre .LE. 199 .OR. nombre .EQ. 200) THEN
    PRINT*, 2
  ELSE
    PRINT*, -(1*2) + 2 * (10 + nombre)
  ENDIF
  PRINT*, (1*2)
END
