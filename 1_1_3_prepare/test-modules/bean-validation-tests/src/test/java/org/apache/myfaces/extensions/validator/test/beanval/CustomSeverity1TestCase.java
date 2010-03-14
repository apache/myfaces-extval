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
import org.apache.myfaces.extensions.validator.test.beanval.view.CustomSeverityTestCase1PageBean;
import org.apache.myfaces.extensions.validator.test.beanval.custom.CustomViolationSeverity;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticInMemoryConfiguration;

import javax.faces.application.FacesMessage;

public class CustomSeverity1TestCase extends BaseBeanValPropertyValidationTestCase<CustomSeverityTestCase1PageBean>
{
    public CustomSeverity1TestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(CustomSeverity1TestCase.class);
    }

    protected CustomSeverityTestCase1PageBean getBeanToTest()
    {
        return new CustomSeverityTestCase1PageBean();
    }

    public void testCustomPayloadViaGlobalProperty()
    {
        ExtValContext.getContext()
                .addGlobalProperty(ViolationSeverity.Warn.class.getName(), CustomViolationSeverity.Warning.class);

        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_WARN);
    }

    public void testCustomPayloadViaStaticConfig()
    {
        StaticInMemoryConfiguration config = new StaticInMemoryConfiguration();
        config.addMapping(ViolationSeverity.Warn.class.getName(), CustomViolationSeverity.Warning.class.getName());

        ExtValContext.getContext()
                .addStaticConfiguration(StaticConfigurationNames.VALIDATION_PARAMETER_CONFIG, config);

        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_WARN);
    }

    public void testUnknownPayload()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }
}
