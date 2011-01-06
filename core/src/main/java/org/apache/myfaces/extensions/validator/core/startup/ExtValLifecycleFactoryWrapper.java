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
package org.apache.myfaces.extensions.validator.core.startup;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.lifecycle.Lifecycle;
import javax.faces.lifecycle.LifecycleFactory;
import java.util.Iterator;

/**
 * Lifecycle factory that wraps the LifeCycle implementation so that we can guarantee that the
 * ExtVal PhaseListeners are 'executed' (initialised) before any code of the JSF implementation is executed.
 * Solution for the issue EXTVAL-123.
 *
 * @author Rudy De Busscher
 * @author Gerard Petracek
 * @since x.x.5
 */
@UsageInformation(value = UsageCategory.INTERNAL)
public class ExtValLifecycleFactoryWrapper extends LifecycleFactory
{

    private final LifecycleFactory wrapped;

    /**
     * Constructs The wrapper around the factory specified as parameter.
     * @param wrapped Factory instance we will wrap.
     */
    public ExtValLifecycleFactoryWrapper(LifecycleFactory wrapped)
    {
        this.wrapped = wrapped;
    }

    public void addLifecycle(String s, Lifecycle lifecycle)
    {
        wrapped.addLifecycle(s, lifecycle);
    }

    public Lifecycle getLifecycle(String s)
    {
        Lifecycle result = this.wrapped.getLifecycle(s);

        return new ExtValLifecycleWrapper(result);
    }

    public Iterator<String> getLifecycleIds()
    {
        return wrapped.getLifecycleIds();
    }
}
