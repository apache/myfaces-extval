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
package org.apache.myfaces.extensions.validator.util;

import java.util.concurrent.ConcurrentHashMap;

/**
 * in some cases we have to store null values to avoid re-evaluation
 *
 * @author Gerhard Petracek
 */
public class NullValueAwareConcurrentHashMap<K, V> extends ConcurrentHashMap<K, V>
{
    private static final long serialVersionUID = -7294527213622879543L;

    private V nullMarkerValue;

    public NullValueAwareConcurrentHashMap(V nullMarkerValue)
    {
        this.nullMarkerValue = nullMarkerValue;
    }

    public NullValueAwareConcurrentHashMap(Class<? extends V> nullMarkerValueClass)
    {
        if(nullMarkerValueClass.getName().equals(Object.class.getName()))
        {
            //noinspection unchecked
            nullMarkerValue = (V)new DefaultNullMarker();
        }
        else if(nullMarkerValueClass.getName().equals(String.class.getName()))
        {
            //noinspection unchecked
            nullMarkerValue = (V)"{null}";
        }
        else if(nullMarkerValueClass.getName().equals(Class.class.getName()))
        {
            //noinspection unchecked
            nullMarkerValue = (V)Void.class.getClass();
        }
        else
        {
            nullMarkerValue = ClassUtils.tryToInstantiateClass(nullMarkerValueClass);
        }
    }

    @Override
    public V put(K key, V value)
    {
        if(value == null)
        {
            return super.put(key, nullMarkerValue);
        }
        return super.put(key, value);
    }

    @Override
    public V get(Object key)
    {
        V result = super.get(key);

        if(nullMarkerValue.equals(result))
        {
            return null;
        }

        return result;
    }

    private class DefaultNullMarker
    {
    }
}
