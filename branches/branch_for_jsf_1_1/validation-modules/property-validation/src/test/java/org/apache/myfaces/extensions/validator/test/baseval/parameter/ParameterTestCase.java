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
package org.apache.myfaces.extensions.validator.test.baseval.parameter;

import junit.framework.TestCase;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.baseval.annotation.Required;

import javax.faces.application.FacesMessage;

public class ParameterTestCase extends TestCase
{
    public static Test suite()
    {
        return new TestSuite(ParameterTestCase.class);
    }

    public void testParameterStyleOne() throws Exception
    {
        ValidationParameterExtractor extractor = new DefaultValidationParameterExtractor();

        TestPerson person = new TestPerson();
        Required required = person.getClass().getDeclaredField("firstName").getAnnotation(Required.class);

        assertNotNull(extractor.extract(required).containsKey(ViolationSeverity.class));
        assertNotNull(extractor.extract(required, ViolationSeverity.class).iterator().next());
        assertEquals(extractor.extract(required, ViolationSeverity.class).iterator().next(), FacesMessage.SEVERITY_WARN);
    }

    public void testParameterStyleTwo() throws Exception
    {
        ValidationParameterExtractor extractor = new DefaultValidationParameterExtractor();

        TestPerson person = new TestPerson();
        Required required = person.getClass().getDeclaredField("lastName").getAnnotation(Required.class);

        assertNotNull(extractor.extract(required).containsKey("client_side_validation_support"));
        assertNotNull(extractor.extract(required, "client_side_validation_support").iterator().next());
        assertEquals(extractor.extract(required, "client_side_validation_support").iterator().next(), false);
    }

    public void testParameterStyleThree() throws Exception
    {
        ValidationParameterExtractor extractor = new DefaultValidationParameterExtractor();

        TestPerson person = new TestPerson();
        Required required = person.getClass().getDeclaredField("lastName").getAnnotation(Required.class);

        assertNotNull(extractor.extract(required).containsKey(TestPriority.class));
        assertNotNull(extractor.extract(required, TestPriority.class).iterator().next());
        assertEquals(extractor.extract(required, TestPriority.class, Integer.class).iterator().next(), new Integer(1));
        assertEquals(extractor.extract(required, TestPriority.class, String.class).size(), 2);
        assertEquals(extractor.extract(required, TestPriority.class, String.class, TestPriority.ShortDescription.class), "do it asap");
        assertEquals(extractor.extract(required, TestPriority.class, String.class, TestPriority.LongDescription.class), "do it immediately");
    }
}
