#ifndef _BBT_H
#define _BBT_H


void bbt_insert_bbt(void *, void *, int (*)());
void bbt_delete_bbt(void *, void *, int (*)());

#define BBT_HEADER(self) int priority; struct self *left, *right


#endif
