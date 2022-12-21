! Temporaries for unknown source codes, to permit compilation + linking
      SUBROUTINE uvset
      RETURN
      END

      SUBROUTINE sbc(g, nx, ny)
      IMPLICIT none
      INTEGER nx, ny
      REAL g(nx, ny)

      RETURN
      END
      SUBROUTINE bcond(g, nx, ny)
      IMPLICIT none
      INTEGER nx, ny
      REAL g(nx, ny)

      RETURN
      END
      SUBROUTINE ubc(u,v)
      IMPLICIT none
      REAL u, v
 
      RETURN
      END
      
      SUBROUTINE isrc(uc, vc, u, v, we, g, gd, fs, fd, h, dx, dy, asv)
      IMPLICIT none
      REAL uc, vc, u, v, we, g, gd, fs, fd, h, dx, dy, asv
      
      RETURN
      END

      SUBROUTINE msrc(qi, qj, h, f, beta, w0, dx, dy)
      IMPLICIT none 
      REAL qi, qj, h, f, beta, w0, dx, dy

      RETURN
      END
