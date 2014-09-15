module mod_namelist
  
  implicit none


contains

  subroutine read_namelist( file_unit )

    use mod_dimensions
    use mod_cntlscm
    use mod_rundata
    use mod_indata
    use mod_logic
    use mod_inmoses
    use mod_inprof
    use mod_inobsfor
    use mod_radcloud
    use mod_physwitch
    use mod_ingeofor

    implicit none
    integer, intent(in) :: file_unit

    rewind( file_unit )
    call init_cntlscm()
    call default_cntlscm()
    read(file_unit, CNTLSCM)

    call set_all_levels_to( model_levels_nml )
    max_nfor = nfor

    rewind(file_unit)
    call init_indata()
    call default_indata()
    read(file_unit, INDATA)

    rewind(file_unit)
    call init_rundata()
    call default_rundata()
    read(file_unit, RUNDATA)

    rewind(file_unit)
    call init_logic()
    call default_logic()
    read(file_unit, LOGIC)

    rewind(file_unit)
    call init_inmoses()
    call default_inmoses()
    read(file_unit, INMOSES)

    rewind(file_unit)
    call init_inprof()
    call default_inprof()
    read(file_unit, INPROF)

    rewind(file_unit)
    call init_inobsfor()
    call default_inobsfor()
    read(file_unit, INOBSFOR)

    rewind(file_unit)
    call init_radcloud()
    call default_radcloud()
    read(file_unit, RADCLOUD)

    rewind(file_unit)
    call init_physwitch()
    call default_physwitch()
    read(file_unit, PHYSWITCH)

    rewind(file_unit)
    call init_ingeofor()
    call default_ingeofor()
    read(file_unit, INGEOFOR)

  end subroutine read_namelist

  subroutine write_namelist( file_unit )

    use mod_dimensions
    use mod_cntlscm
    use mod_rundata
    use mod_indata
    use mod_logic
    use mod_inmoses
    use mod_inprof
    use mod_inobsfor
    use mod_radcloud
    use mod_physwitch
    use mod_ingeofor

    implicit none

    integer, intent(in) :: file_unit

    write(file_unit, CNTLSCM)
    write(file_unit, INDATA)
    write(file_unit, RUNDATA)
    write(file_unit, LOGIC)
    write(file_unit, INMOSES)
    write(file_unit, INPROF)
    write(file_unit, INOBSFOR)
    write(file_unit, RADCLOUD)
    write(file_unit, PHYSWITCH)
    write(file_unit, INGEOFOR)

  end subroutine write_namelist

end module mod_namelist
