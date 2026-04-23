Within tables: delete rows, working with column blocks
------------------------------------------------------

Having already obtained a symbol table form the interpretation the
equations, a **subsequent modification of the equations may result in
new symbols or other symbols may become redundant**, which leads to
changes in the symbol table.

**Additionally occurring new symbols** are indicated in the symbol table
by **green colored rows**. For these, the columns *Unit* and *Meaning*
may be edited.

Some **symbols may have become redundant**, because they are no longer
used; the corresponding symbol rows are shifted to the end of the table
and indicated by **yellow color**. Here, the user must decide whether
these rows can be deleted; if so, these can be deleted by clicking the
toolbar icon |delete-row|.

.. |delete-row| image:: /icons/DeleteRow_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

At present, **Column blocks** cannot be defined within tables;
corresponding blocks of data can therefore not be exported to, e.g.,
Excel. It is however possible by “copy and paste” to **insert such a
column block in an UR table** which has been selected in Excel and
copied to the Windows clipboard. This requires the following steps:

-  open the upper-left cell of the UR table by double-clicking into it;

-  insert the block into this cell using “paste” in the mouse context
   menu; In this moment, this cell holds the whole block.

-  pressing the enter button once extends this block over the area of
   cells.