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
package org.apache.myfaces.extensions.validator.core.renderkit;

/**
 * @author Gerhard Petracek
 */
public class ConvertedValueCache
{
    private static ThreadLocal<ConvertedValueCacheEntry> value = new ThreadLocal<ConvertedValueCacheEntry>();

    public static void reset()
    {
        value.set(null);
        value.remove();
    }

    //needed because null is a valid value
    static boolean isCachedValueAvailable()
    {
        return getCacheEntry().isCachedValueAvailable();
    }

    static Object getCachedValue()
    {
        return getCacheEntry().getCachedValue();
    }

    static void setCachedValue(Object convertedObject)
    {
        getCacheEntry().setCachedValue(convertedObject);
    }

    private static ConvertedValueCacheEntry getCacheEntry()
    {
        ConvertedValueCacheEntry entry = value.get();

        if(entry == null)
        {
            entry = new ConvertedValueCacheEntry();
            value.set(entry);
        }
        return entry;
    }
}
