
module solver
    implicit none
contains
    pure integer(8) function gcd(a, b)
        integer(8), intent(in) :: a, b
        integer(8) :: x, y, r
        x = a
        y = b
        do while (y /= 0_8)
            r = mod(x, y)
            x = y
            y = r
        end do
        gcd = x
    end function gcd

    integer(8) function solve_279_fortran(limit) bind(C, name="solve_279_fortran")
        use iso_c_binding
        integer(8), intent(in), value :: limit
        integer(8) :: ans
        integer(8) :: m, n, p

        ! 正三角形の数 (60度)
        ans = limit / 3_8

        ! 1. 90度 (直角三角形)
        m = 2_8
        do while (2_8 * m * (m + 1_8) <= limit)
            do n = 1_8 + mod(m, 2_8), m - 1_8, 2_8
                p = 2_8 * m * (m + n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        ! 2. 120度
        m = 2_8
        do while ((2_8 * m + 1_8) * (m + 1_8) <= limit)
            do n = 1_8, m - 1_8
                if (mod(m - n, 3_8) == 0_8) cycle
                p = (2_8 * m + n) * (m + n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        ! 3. 60度 Type 1
        m = 2_8
        do while ((2_8 * m + 1_8) * (m + 2_8) <= limit)
            do n = 1_8, m - 1_8
                if (mod(m - n, 3_8) == 0_8) cycle
                p = (2_8 * m + n) * (m + 2_8 * n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        ! 4. 60度 Type 2
        m = 2_8
        do while (3_8 * m * (m + 1_8) <= limit)
            do n = 1_8, m - 1_8
                if (mod(m - n, 3_8) == 0_8) cycle
                p = 3_8 * m * (m + n)
                if (p > limit) exit
                if (gcd(m, n) == 1_8) then
                    ans = ans + limit / p
                end if
            end do
            m = m + 1_8
        end do

        solve_279_fortran = ans
    end function solve_279_fortran
end module solver
