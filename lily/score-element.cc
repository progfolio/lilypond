/*
  score-elem.cc -- implement Score_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <string.h>

#include "p-score.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "score-element.hh"
#include "debug.hh"
#include "spanner.hh"
#include "line-of-score.hh"
#include "item.hh"
#include "p-col.hh"
#include "molecule.hh"
#include "misc.hh"
#include "paper-outputter.hh"

Score_element::Score_element()
{
  output_p_ =0;
  break_helper_only_b_ = false;
  transparent_b_ = false;
  pscore_l_=0;
  status_i_ = 0;
  element_property_alist_ = SCM_EOL;
}

Score_element::Score_element (Score_element const&s)
  :   Graphical_element (s)
{
  /* called from derived ctor, so most info points to the same deps
     as (Directed_graph_node&)s. Nobody points to us, so don't copy
     dependents.      
   */
  

  // deep copy ?
  element_property_alist_ = s.element_property_alist_;
  dependency_arr_ = s.dependency_arr_;
  output_p_ =0;
  break_helper_only_b_ = s.break_helper_only_b_;
  transparent_b_ = s.transparent_b_;
  status_i_ = s.status_i_;
  pscore_l_ = s.pscore_l_;
}

Score_element::~Score_element()
{
  delete output_p_; 
  assert (status_i_ >=0);
  status_i_  = -1;
}

Score_element*
Score_element::dependency (int i) const
{
  return dependency_arr_ [i];
}

int
Score_element::dependency_size () const
{
  return dependency_arr_.size ();
}



SCM
Score_element::get_elt_property (SCM s)
{
  return scm_assq(s, element_property_alist_);
}
void
Score_element::set_elt_property (SCM s, SCM v)
{
  element_property_alist_ =
    scm_assoc_set_x (element_property_alist_, s, v);
}

Interval
Score_element::do_width() const 
{
  Interval r;

  Molecule*m = output_p_ ?  output_p_ : do_brew_molecule_p();
  r = m->extent().x ();

  if (!output_p_)
    delete m;
  
  return r;
}

Interval
Score_element::do_height() const 
{
  Interval r;
  Molecule*m = output_p_ ?  output_p_ : do_brew_molecule_p();
  r = m->extent().y ();
  if (!output_p_)
    delete m;

  return r;
}


/*
  STANDARD METHS
 */
void
Score_element::print() const
{
#ifndef NPRINT
  DOUT << classname(this) << "{\n";
  DOUT << "dependencies: " << dependency_size();
 
  Graphical_element::do_print ();
  do_print();
  
  DOUT <<  "}\n";
#endif
}


Paper_def*
Score_element::paper()  const
{
  return pscore_l_->paper_l_;
}


Lookup const *
Score_element::lookup_l () const
{
  SCM sz = scm_assq (ly_symbol ("fontsize"), element_property_alist_);
  if (sz != SCM_BOOL_F)
    return pscore_l_->paper_l_->lookup_l (gh_scm2int (SCM_CDR (sz)));
  else
    return pscore_l_->paper_l_->lookup_l (0);
}

void
Score_element::add_processing()
{
  assert (status_i_ >=0);
  if (status_i_)
    return;
  status_i_ ++;
  do_add_processing();
}


void
Score_element::calculate_dependencies (int final, int busy,
				    Score_element_method_pointer funcptr)
{
  assert (status_i_ >=0);

  if (status_i_ >= final)
    return;

  assert (status_i_!= busy);
  status_i_= busy;

  for (int i=0; i < dependency_size(); i++)
    dependency (i)->calculate_dependencies (final, busy, funcptr);

  Link_array<Score_element> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->calculate_dependencies (final, busy, funcptr);
  
  invalidate_cache (X_AXIS);
  invalidate_cache (Y_AXIS);
  (this->*funcptr)();
  status_i_= final;
}

void
Score_element::output_processing () 
{
  if (transparent_b_)
    return;
  if (output_p_)
    delete output_p_;
  
  output_p_ = do_brew_molecule_p ();
  pscore_l_->outputter_l_->output_molecule (output_p_,
					    absolute_offset (),
					    classname(this));
}

/*
  
  VIRTUAL STUBS

 */

void
Score_element::do_break_processing()
{
  handle_broken_dependencies();
}

void
Score_element::do_post_processing()
{
}

void
Score_element::do_breakable_col_processing()
{
  handle_prebroken_dependencies();
}

void
Score_element::do_pre_processing()
{
}

void
Score_element::do_space_processing ()
{
}

void
Score_element::do_add_processing()
{
}

void
Score_element::do_substitute_element_pointer (Score_element*,Score_element*)
{
}


Molecule*
Score_element::do_brew_molecule_p() const
{
  Molecule a (lookup_l ()->fill (Box (Interval (0,0), Interval (0,0))));
  return new Molecule (a);
}


Line_of_score *
Score_element::line_l() const
{
  return 0;
}

/*
  
  DEPENDENCIES

  */

void
Score_element::remove_dependency (Score_element*e)
{
  int i;
  while ((i = dependency_arr_.find_i (e)) >=0 )
    dependency_arr_.unordered_del (i);

  substitute_dependency (e, 0);
}

void
Score_element::add_dependency (Score_element*e)
{
  if (e)
    dependency_arr_.push (e);
  else
    warning("Null dependency added");
      
}
void
Score_element::substitute_dependency (Score_element* old, Score_element* new_l)
{
  do_substitute_element_pointer (old,new_l);
  old->do_substitute_element_pointer (this, 0);
}

void
Score_element::handle_broken_dependencies()
{
  Line_of_score *line  = line_l();
  if (!line)
    return;

  Link_array<Score_element> remove_us_arr;
  for (int i=0; i < dependency_size(); i++) 
    {
      Score_element * elt = dependency (i);
      if (elt->line_l() != line)
	{
	  if (Spanner *sp = dynamic_cast<Spanner *> (elt)) 
	    {
	      Spanner * broken = sp->find_broken_piece (line);
	      substitute_dependency (sp, broken);

	      add_dependency (broken);
	    }
	  else if (Item *original = dynamic_cast <Item *> (elt))
	    {
	      Item * my_item = original->find_prebroken_piece (line);
		
	      substitute_dependency (elt, my_item);
	      if (my_item)
		add_dependency (my_item);
	    }
	  remove_us_arr.push (elt);
	}
    }

  remove_us_arr.default_sort();
  remove_us_arr.uniq();
  for (int i=0;  i <remove_us_arr.size(); i++)
    remove_dependency (remove_us_arr[i]);
}

/*
  This sux.

  unlike with spanners, the number of items can increase

  span: item1

  becomes

  span: item1 item2 item3

  How to let span (a derived class) know that this happened?
 */
void
Score_element::handle_prebroken_dependencies()
{
  Link_array<Score_element> old_arr, new_arr;
  
  for (int i=0; i < dependency_size(); i++) 
    {
      Score_element * elt = dependency (i);
      Item *it_l = dynamic_cast <Item *> (elt);
      if (it_l && it_l->breakable_b_)
	if (Item *me = dynamic_cast<Item*> (this) )
	  {
	    Score_element *new_l = it_l->find_prebroken_piece (me->break_status_dir_);
	    if (new_l != elt) 
	      {
		new_arr.push (new_l);
		old_arr.push (elt);
	      }
	  }
	else 
	  {
	    new_arr.push (it_l->broken_to_drul_[LEFT]);
	    old_arr.push (0);
	    old_arr.push (0);		
	    new_arr.push (it_l->broken_to_drul_[RIGHT]);		
	  }
    }
  
  for (int i=0;  i < old_arr.size(); i++)
    if (old_arr[i])
      substitute_dependency (old_arr[i], new_arr[i]);
}

void
Score_element::handle_prebroken_dependents()
{
}



Link_array<Score_element>
Score_element::get_extra_dependencies() const
{
  Link_array<Score_element> empty;
  return empty;
}

bool
Score_element::linked_b() const
{
  return get_extra_dependencies().size() || 
    dependency_size();
}
void
Score_element::do_print () const
{
}
