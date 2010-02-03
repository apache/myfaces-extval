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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.Comparator;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class InvocationOrderComparator<T> implements Comparator<T>
{
    public int compare(T nm1, T nm2)
    {
        if (hasPriority(nm1) && hasPriority(nm2))
        {
            return isPriorityHigher(nm1.getClass().getAnnotation(InvocationOrder.class),
                    nm2.getClass().getAnnotation(InvocationOrder.class));
        }
        if (!hasPriority(nm1) && !hasPriority(nm2))
        {
            return 0;
        }
        return hasPriority(nm1) ? -1 : 1;
    }

    private int isPriorityHigher(InvocationOrder priority1, InvocationOrder priority2)
    {
        if (priority1.value() == priority2.value())
        {
            return 0;
        }

        return priority1.value() < priority2.value() ? -1 : 1;
    }

    private boolean hasPriority(Object nm)
    {
        return nm.getClass().isAnnotationPresent(InvocationOrder.class);
    }
}