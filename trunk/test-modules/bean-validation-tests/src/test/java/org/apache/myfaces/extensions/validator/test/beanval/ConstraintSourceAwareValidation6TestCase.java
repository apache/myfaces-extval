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

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.test.beanval.model.ConstraintSourceAware6Bean;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomTargetProperty;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomTargetPropertyId;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;
import org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId;

import javax.faces.application.FacesMessage;

/**
 * EXTVAL-83
 *
 * @author Gerhard Petracek
 */
public class ConstraintSourceAwareValidation6TestCase extends BaseBeanValPropertyValidationTestCase<ConstraintSourceAware6Bean>
{
    public ConstraintSourceAwareValidation6TestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(ConstraintSourceAwareValidation6TestCase.class);
    }

    protected ConstraintSourceAware6Bean getBeanToTest()
    {
        return new ConstraintSourceAware6Bean();
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        ExtValContext extValContext = ExtValContext.getContext();

        extValContext.addGlobalProperty(ConstraintSource.class.getName(), CustomConstraintSource.class);
        extValContext.addGlobalProperty(IgnoreConstraintSource.class.getName(), CustomIgnoreConstraintSource.class);
        extValContext.addGlobalProperty(TargetProperty.class.getName(), CustomTargetProperty.class);
        extValContext.addGlobalProperty(TargetPropertyId.class.getName(), CustomTargetPropertyId.class);
    }

    public void testCustomAnnotations1()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testCustomAnnotations2()
    {
        createValueBindingForComponent(this.inputComponent2, "#{testBean.property2}");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testCustomAnnotations3()
    {
        createValueBindingForComponent(this.inputComponent3, "#{testBean.property3}");
        setValueToValidate(this.inputComponent3, "");

        validateComponents();

        assertComponentValid(this.inputComponent3);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }
}
