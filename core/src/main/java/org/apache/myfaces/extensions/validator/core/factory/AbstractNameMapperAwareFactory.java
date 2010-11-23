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
package org.apache.myfaces.extensions.validator.core.factory;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.InvocationOrderComparator;

import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * Default implementation of a NameMapperAwareFactory where the concrete implementation is responsible for the storage
 * of the list of nameMappers.
 *
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.API)
public abstract class AbstractNameMapperAwareFactory<T> implements NameMapperAwareFactory<NameMapper<T>>
{
    private List<Class> deniedNameMapperList = new CopyOnWriteArrayList<Class>();

    public synchronized void register(NameMapper<T> nameMapper)
    {
        if(!deniedNameMapperList.contains(nameMapper.getClass()))
        {
            getNameMapperList().add(nameMapper);
            List<NameMapper<T>> nameMapperList = getNameMapperList();

            if(nameMapperList instanceof CopyOnWriteArrayList)
            {
                List<NameMapper<T>> sortableList = new ArrayList<NameMapper<T>>(nameMapperList);
                Collections.sort(sortableList, getComparator());
                nameMapperList.clear();
                nameMapperList.addAll(sortableList);
            }
            else
            {
                Collections.sort(nameMapperList, getComparator());
            }
        }
    }

    protected Comparator<NameMapper<T>> getComparator()
    {
        return new InvocationOrderComparator<NameMapper<T>>();
    }

    public synchronized void deregister(Class<? extends NameMapper> classToDeregister)
    {
        Iterator<NameMapper<T>> nameMapperIterator = getNameMapperList().iterator();
        while(nameMapperIterator.hasNext())
        {
            if(nameMapperIterator.next().getClass().getName().equals(classToDeregister.getName()))
            {
                nameMapperIterator.remove();
                //don't break - e.g. to deregister all wrappers...
                //break;
            }
        }
    }

    public void deny(Class<? extends NameMapper> classToDeny)
    {
        deregister(classToDeny);

        synchronized (getClass())
        {
            deniedNameMapperList.add(classToDeny);
        }
    }

    protected abstract List<NameMapper<T>> getNameMapperList();
}
