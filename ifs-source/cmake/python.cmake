ecbuild_find_python(VERSION 3.6 REQUIRED NO_LIBS)

# Loop over all python packages and install them
file( GLOB python_packages RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/python python/* )
foreach( python_package ${python_packages} )

  # Copy Python source code to the build directory.
  file( COPY python/${python_package} DESTINATION "./python" )

  # Install the python package using pip, targeting the lib/python directory
  # of the install.
  install( CODE "message( \"Installing Python package ${python_package}:
      ${PYTHON_EXECUTABLE} -m pip install --target=${CMAKE_INSTALL_PREFIX}/lib/python\" )
                 execute_process( COMMAND ${PYTHON_EXECUTABLE} -m pip install --target=${CMAKE_INSTALL_PREFIX}/lib/python ./${python_package}
                                  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/python )" )

endforeach()
