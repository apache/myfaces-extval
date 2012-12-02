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

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.beanval.model.ConstraintSourceAware1MetaDataBean;
import org.junit.Assert;
import org.junit.Test;

/**
 */
public class NotNullValidationTestCase extends BaseBeanValPropertyValidationTestCase<ConstraintSourceAware1MetaDataBean>
{

    @Override
    protected ConstraintSourceAware1MetaDataBean getBeanToTest()
    {
        return new ConstraintSourceAware1MetaDataBean();
    }

    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();
        ((DefaultExtValCoreConfiguration) ExtValCoreConfiguration.get()).overruleActivateRequiredInitialization(true, true);
    }

    @Test
    public void testNotNullValidation()
    {
        // This tests is not testing that isn't already tested somewhere else.  Just to see if the 'config of the test object is ok.
        createValueBindingForComponent(inputComponent1, "#{testBean.property2}");
        setValueToValidate(inputComponent1, "");
        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);

    }

    @Test
    public void testInitialization() throws Exception
    {
        // This will trigger the ValidationStrategyToMetaDataTransformerSubMapperAwareNameMapper what we really liked to test
        createValueBindingForComponent(inputComponent1, "#{testBean.property2}");
        inputComponent1.encodeAll(facesContext);
        Assert.assertTrue(inputComponent1.isRequired());
    }
}
