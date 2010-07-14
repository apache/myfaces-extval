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
package org.apache.myfaces.extensions.validator.test.beanval;

import org.apache.myfaces.extensions.validator.test.base.AbstractExValTestCase;
import org.apache.myfaces.extensions.validator.beanval.startup.BeanValidationStartupListener;
import org.apache.myfaces.extensions.validator.beanval.validation.ModelValidationPhaseListener;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;

public class AbstractBeanValidationTestCase extends AbstractExValTestCase
{
    public AbstractBeanValidationTestCase(String name)
    {
        super(name);
    }

    protected void invokeStartupListeners()
    {
        new BeanValidationStartupListener() {

            private static final long serialVersionUID = -3124182355444754497L;

            @Override
            protected void init()
            {
                super.initModuleConfig();
                super.init();
            }
        }.init();
    }

    protected void processModelValidation()
    {
        new ModelValidationPhaseListener().afterPhase(new PhaseEvent(facesContext, PhaseId.UPDATE_MODEL_VALUES, lifecycle));
    }
}
