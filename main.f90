program main
!==============================================================================#
! MAIN
!------------------------------------------------------------------------------#
! Author:  Edward Higgins <ed.higgins@york.ac.uk>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2022-07-29
!------------------------------------------------------------------------------#
! This code is distributed under the GLWT license.
!==============================================================================#
  use iso_fortran_env
  implicit none

  integer, parameter :: dp = selected_real_kind(15,300)
  integer, parameter :: rate = 44100
  integer, parameter :: wp = int16
  real(kind=dp),parameter      :: pi = 3.14159265358979_dp
  integer :: audio = 11

  integer(kind=wp)  :: sample
  real(kind=dp)     :: t, dt
  real(kind=dp)     :: notes(6,2)
  character(1024) :: stdout
  integer :: i, j, k

  real(kind=dp)   :: note_duration = 1/4.0_dp

  dt = 1.0_dp/rate
  t = 0

  notes(:,1) = [440.0_dp, 329.61_dp, 293.66_dp, 277.18_dp, 293.66_dp, 329.63_dp]
  notes(:,2) = [440.0_dp, 293.66_dp, 277.18_dp, 246.94_dp, 277.18_dp, 293.66_dp]


  call execute_command_line("rm /tmp/audio.pipe")
  call execute_command_line("mkfifo /tmp/audio.pipe")
  call execute_command_line("aplay -r44100 -f S16_LE < /tmp/audio.pipe &")

  open(audio, file = "/tmp/audio.pipe", access = 'stream', action = 'write')

  j=1
  do k = 1, 2
    do i=1, 24
      t=0
      do while (t < note_duration)
        write(audio)  int(0.5*huge(1_wp) * (exp(-10*t) * sin(2*pi*notes(j,k)*t)), kind=wp)
        t = t + dt
      end do
      j = mod(j,6) + 1
    end do
  end do
  close(audio)



end program main
