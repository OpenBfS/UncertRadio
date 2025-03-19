Viewing an existing project
---------------------------

To become acquainted with the functioning of the program it is
recommended to look into existing example projects. The sequence of
steps and the buttons and tabs to be clicked as shown below is nearly
the same when creating a new project.

.. tip::

   User guidance is presented in the last field of the status bar at the
   bottom of the UncertRadio window.


Load an existing project file (extensions txp or csv) of a measurement problem
with the |project-open_icon| button, whereby all calculations are done and,
finally, the :ref:`tab “results”` becomes visible.

To proceed, follow these steps:

* Select the :ref:`tab “procedure”` for comments on that procedure
* Select the :ref:`tab “equations”`
* Click the button "Load symbols from equations"
* Click the button "Load symbols from finalized symbol table"
* Click the button "Accept all" to enable the
  :ref:`tab “values, uncertainties”`


Next, review the equations and the table of symbols, then:

* Select the :ref:`tab “values, uncertainties”`
* Click the Button "Calculation of uncertainties" to enable
  the :ref:`tab “uncertainty budget”`
* Review the details in the uncertainty table
* Select the :ref:`tab “uncertainty budget”` to view
  the uncertainty budget of the output quantity
* Select the :ref:`tab “results”` to view all results,
  including Decision threshold and Detection limit

Alternatively, you can start a Monte Carlo simulation of the
calculations by clicking the "Start" button.

To close the project file, load the next project, or terminate
the program, use the following options:

* File – Quit program to terminate the program
* Load the next project by following the same steps as above
* Close the project file using the |project-close| option or
  terminating the program: File – Quit program.


.. attention::

   If one has changed values while going through the different parts of
   the program, one should NOT save the project when the program asks
   for it, because otherwise the project file would be modified.

Scrolling within tables or in the equations field is possible with the
mouse wheel after clicking within that table of field.

.. |project-open_icon| image:: /icons/document-open.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |project-close| image:: /icons/application-exit.png
    :height: 2ex
    :align: middle
    :class: no-scaled-link
