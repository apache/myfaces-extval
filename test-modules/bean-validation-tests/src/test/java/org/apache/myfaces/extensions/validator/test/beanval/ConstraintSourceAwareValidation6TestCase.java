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

import java.lang.annotation.Annotation;

import javax.faces.application.FacesMessage;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomTargetProperty;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomTargetPropertyId;
import org.apache.myfaces.extensions.validator.test.beanval.model.ConstraintSourceAware6Bean;
import org.junit.Test;

/**
 * EXTVAL-83
 *
 * @author Gerhard Petracek
 */
public class ConstraintSourceAwareValidation6TestCase extends
        BaseBeanValPropertyValidationTestCase<ConstraintSourceAware6Bean>
{
    protected ConstraintSourceAware6Bean getBeanToTest()
    {
        return new ConstraintSourceAware6Bean();
    }

    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        return new DefaultExtValCoreConfiguration()
        {
            @Override
            public Class<? extends Annotation> constraintSourceAnnotation()
            {
                return CustomConstraintSource.class;
            }

            @Override
            public Class<? extends Annotation> ignoreConstraintSourceAnnotation()
            {
                return CustomIgnoreConstraintSource.class;
            }

            @Override
            public Class<? extends Annotation> targetPropertyAnnotation()
            {
                return CustomTargetProperty.class;
            }

            @Override
            public Class<? extends Annotation> targetPropertyIdAnnotation()
            {
                return CustomTargetPropertyId.class;
            }

        };
    }

    @Test
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

    @Test
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

    @Test
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
