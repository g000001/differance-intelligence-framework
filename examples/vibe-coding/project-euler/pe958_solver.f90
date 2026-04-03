
module solver
    implicit none
    
    type :: State
        integer(8) :: a, b, c, d
        integer :: S
    end type State
    
    integer(8) :: target_n
    integer :: MAX_S_HALF = 24
    
    type(State), allocatable, dimension(:) :: states
    integer :: state_count
    integer :: max_states = 15000000

contains

    recursive subroutine generate(a, b, c, d, S)
        integer(8), intent(in) :: a, b, c, d
        integer, intent(in) :: S
        integer :: q
        integer(8) :: na, nc
        
        state_count = state_count + 1
        states(state_count)%a = a
        states(state_count)%b = b
        states(state_count)%c = c
        states(state_count)%d = d
        states(state_count)%S = S
        
        do q = 1, MAX_S_HALF - S
            na = q * a + b
            nc = q * c + d
            if (na > target_n) exit
            call generate(na, a, nc, c, S + q)
        end do
    end subroutine generate

    integer function compare(s1, s2)
        type(State), intent(in) :: s1, s2
        if (s1%a < s2%a) then
            compare = -1
        else if (s1%a > s2%a) then
            compare = 1
        else
            if (s1%c < s2%c) then
                compare = -1
            else if (s1%c > s2%c) then
                compare = 1
            else
                compare = 0
            end if
        end if
    end function compare

    recursive subroutine quicksort(low, high)
        integer, intent(in) :: low, high
        integer :: i, j
        type(State) :: pivot, temp
        if (low < high) then
            pivot = states((low + high) / 2)
            i = low
            j = high
            do
                do while (compare(states(i), pivot) < 0)
                    i = i + 1
                end do
                do while (compare(states(j), pivot) > 0)
                    j = j - 1
                end do
                if (i <= j) then
                    temp = states(i)
                    states(i) = states(j)
                    states(j) = temp
                    i = i + 1
                    j = j - 1
                end if
                if (i > j) exit
            end do
            if (low < j) call quicksort(low, j)
            if (i < high) call quicksort(i, high)
        end if
    end subroutine quicksort

    integer function binary_search(x, z)
        integer(8), intent(in) :: x, z
        integer :: low, high, mid
        
        low = 1
        high = state_count
        binary_search = 0
        
        do while (low <= high)
            mid = (low + high) / 2
            if (states(mid)%a == x) then
                if (states(mid)%c == z) then
                    binary_search = mid
                    return
                else if (states(mid)%c < z) then
                    low = mid + 1
                else
                    high = mid - 1
                end if
            else if (states(mid)%a < x) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
    end function binary_search

    integer(8) function floor_div(num, den)
        integer(8) :: num, den
        integer(8) :: n, d
        n = num
        d = den
        if (d < 0) then
            n = -n
            d = -d
        end if
        if (n >= 0) then
            floor_div = n / d
        else
            floor_div = -((-n + d - 1) / d)
        end if
    end function floor_div

    integer(8) function solve_958_fortran(n) bind(C, name="solve_958_fortran")
        use iso_c_binding
        integer(8), intent(in), value :: n
        
        integer :: i, idx
        integer(8) :: a, b, c, d
        integer :: S_L, S_R, total_S
        integer :: min_S
        integer(8) :: min_m, m
        integer(8) :: x0, z0, x, z
        integer(8) :: k_min, k_max, k
        
        target_n = n
        allocate(states(max_states))
        state_count = 0
        
        ! 1. 左半分の状態を事前生成
        call generate(1_8, 0_8, 0_8, 1_8, 0)
        
        ! 2. 高速検索のためにソート
        call quicksort(1, state_count)
        
        min_S = 999999
        min_m = 9223372036854775807_8
        
        ! 3. 右半分との結合 (Meet-in-the-Middle)
        do i = 1, state_count
            a = states(i)%a
            b = states(i)%b
            c = states(i)%c
            d = states(i)%d
            S_L = states(i)%S
            
            ! 逆行列による特解の導出 (ad - bc = 1 or -1)
            if (a * d - b * c == 1_8) then
                x0 = d * n
                z0 = -c * n
            else
                x0 = -d * n
                z0 = c * n
            end if
            
            ! kの範囲を厳密に計算 (x > 0, z >= 0)
            if (b > 0) then
                k_min = floor_div(-x0 + 1, b)
            else
                k_min = -1000000000_8
            end if
            
            if (a > 0) then
                k_max = floor_div(z0, a)
            else
                k_max = 1000000000_8
            end if
            
            do k = k_min, k_max
                x = x0 + k * b
                z = z0 - k * a
                if (x <= 0 .or. z < 0) cycle
                if (x > n) cycle
                
                idx = binary_search(x, z)
                if (idx > 0) then
                    S_R = states(idx)%S
                    total_S = S_L + S_R
                    m = c * x + d * z
                    
                    if (total_S < min_S) then
                        min_S = total_S
                        min_m = m
                    else if (total_S == min_S .and. m < min_m) then
                        min_m = m
                    end if
                end if
            end do
        end do
        
        deallocate(states)
        solve_958_fortran = min_m
    end function solve_958_fortran
end module solver
