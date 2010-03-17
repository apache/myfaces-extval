/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.metadata.transformer;

import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.SubMapperAwareNameMapper;
import org.apache.myfaces.extensions.validator.core.Nested;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import java.util.List;
import java.util.Collections;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.ListIterator;


/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation({UsageCategory.INTERNAL})
class SortedNameMapperList<T extends NameMapper> implements List<T>
{
    private List<T> wrapped;
    private List<T> globalSubNameMapperList;

    SortedNameMapperList(List<T> wrapped, List<T> subNameMapperList)
    {
        this.wrapped = wrapped;
        this.globalSubNameMapperList = subNameMapperList;
    }

    public boolean add(T t)
    {
        if (t != null && t.getClass().isAnnotationPresent(Nested.class))
        {
            return addSubNameMapper(t);
        }
        else
        {
            boolean result = wrapped.add(t);

            Collections.sort(wrapped, new Comparator<T>()
            {
                public int compare(T nm1, T nm2)
                {
                    if (nm1 instanceof SubMapperAwareNameMapper && nm2 instanceof SubMapperAwareNameMapper)
                    {
                        return 0;
                    }
                    return nm1 instanceof SubMapperAwareNameMapper ? 1 : -1;
                }
            });
            return result;
        }
    }

    @SuppressWarnings({"unchecked"})
    private boolean addSubNameMapper(T subNameMapper)
    {
        boolean result = false;
        for (NameMapper nameMapper : this.wrapped)
        {
            if (nameMapper instanceof SubMapperAwareNameMapper)
            {
                ((SubMapperAwareNameMapper) nameMapper).addNameMapper(subNameMapper);
                result = true;
            }
        }

        tryToAddMapperAsGlobalSubNameMapper(subNameMapper);
        return result;
    }

    private void tryToAddMapperAsGlobalSubNameMapper(T subNameMapper)
    {
        if(!this.globalSubNameMapperList.contains(subNameMapper))
        {
            this.globalSubNameMapperList.add(subNameMapper);
        }
    }

    /*
     * generated
     */
    public int size()
    {
        return wrapped.size();
    }

    public boolean isEmpty()
    {
        return wrapped.isEmpty();
    }

    public boolean contains(Object o)
    {
        return wrapped.contains(o);
    }

    public Iterator<T> iterator()
    {
        return wrapped.iterator();
    }

    public Object[] toArray()
    {
        return wrapped.toArray();
    }

    @SuppressWarnings({"SuspiciousToArrayCall"})
    public <T> T[] toArray(T[] a)
    {
        return wrapped.toArray(a);
    }

    public boolean remove(Object o)
    {
        return wrapped.remove(o);
    }

    public boolean containsAll(Collection<?> c)
    {
        return wrapped.containsAll(c);
    }

    public boolean addAll(Collection<? extends T> c)
    {
        return wrapped.addAll(c);
    }

    public boolean addAll(int index, Collection<? extends T> c)
    {
        return wrapped.addAll(index, c);
    }

    public boolean removeAll(Collection<?> c)
    {
        return wrapped.removeAll(c);
    }

    public boolean retainAll(Collection<?> c)
    {
        return wrapped.retainAll(c);
    }

    public void clear()
    {
        wrapped.clear();
    }

    public T get(int index)
    {
        return wrapped.get(index);
    }

    public T set(int index, T element)
    {
        return wrapped.set(index, element);
    }

    public void add(int index, T element)
    {
        wrapped.add(index, element);
    }

    public T remove(int index)
    {
        return wrapped.remove(index);
    }

    public int indexOf(Object o)
    {
        return wrapped.indexOf(o);
    }

    public int lastIndexOf(Object o)
    {
        return wrapped.lastIndexOf(o);
    }

    public ListIterator<T> listIterator()
    {
        return wrapped.listIterator();
    }

    public ListIterator<T> listIterator(int index)
    {
        return wrapped.listIterator(index);
    }

    public List<T> subList(int fromIndex, int toIndex)
    {
        return wrapped.subList(fromIndex, toIndex);
    }
}