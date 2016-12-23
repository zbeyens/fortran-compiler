PROGRAM factorielle

! Compute the factorial of a number.
* If the input number is negative, print -1.

  INTEGER nombre, resultat, compteur
  READ*, nombre
  resultat = 1

  IF (.NOT. nombre .GE. 0 .AND. nombre .LE. 100) THEN
    DO compteur = 1, 10
      resultat = resultat * compteur
    ENDDO
    PRINT*, resultat
  ENDIF
  PRINT*, -(1*2)
  IF (nombre .GT. 100 .AND. nombre .LE. 199 .OR. nombre .EQ. 200) THEN
    PRINT*, 2
  ELSE
    PRINT*, -(1*2) + 2 * (10 + nombre)
  ENDIF
  PRINT*, -(1*2)
END
