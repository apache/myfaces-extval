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

import javax.faces.FacesException;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseListener;
import javax.faces.lifecycle.Lifecycle;

/**
 * Wrapper around a Lifecycle that initialise the ExtVal PhaseListeners before an execution of any
 * phase (except the render view). Solution for the issue EXTVAL-123.
 *
 * @author Rudy De Busscher
 * @author Gerard Petracek
 * @since x.x.5
 */
@UsageInformation(value = UsageCategory.INTERNAL)
class ExtValLifecycleWrapper extends Lifecycle
{
    private Lifecycle wrapped;

    private boolean initialized = false;

    ExtValLifecycleWrapper(Lifecycle wrapped)
    {
        this.wrapped = wrapped;
    }

    public void addPhaseListener(PhaseListener phaseListener)
    {
        wrapped.addPhaseListener(phaseListener);
    }

    public void execute(FacesContext facesContext)
            throws FacesException
    {
        if(!this.initialized)
        {
            initializeExtVal();
        }
        wrapped.execute(facesContext);
    }

    public PhaseListener[] getPhaseListeners()
    {
        return wrapped.getPhaseListeners();
    }

    public void removePhaseListener(PhaseListener phaseListener)
    {
        wrapped.removePhaseListener(phaseListener);
    }

    public void render(FacesContext facesContext)
            throws FacesException
    {
        // Don't need to initialize ExtVal here because this phase is never the very first one in a system (restore
        // view is always the first)
        wrapped.render(facesContext);
    }


    private synchronized void initializeExtVal()
    {
        //we don't need further checks - startup listeners are deregistered after the invocation.
        for (PhaseListener currentPhaseListener : getPhaseListeners())
        {
            if (currentPhaseListener instanceof AbstractStartupListener)
            {
                currentPhaseListener.beforePhase(null);
            }
        }
        this.initialized = true;
    }
}
